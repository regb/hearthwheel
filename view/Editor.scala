package hearthwheel
package core

import scala.collection.mutable

import sgl.Input
import sgl.Input.Keys
import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d._
import sgl.engine.gameobject.spatial2d._
import hearthwheel.model._
import hearthwheel.model.MathUtil._
import sgl.geometry.Vec2

/** The maps made by hand, and new ones. */
object EditorMenuScene extends Scene {
  override def load(context: GameContext)(using scope: Scope): Unit = {
    val services = context.global[Services]
    val profile = context.global[Profile]
    val pointer = new Pointer(new Ui(context.global[Gallery], () => services.play(SoundKind.Click, 0.5f * profile.soundVolume, 1f)), services)
    var size = 1
    var seed = 1L + scala.math.abs(services.seed() % 9999L)
    Input.setInputProcessor(pointer.controls, Set(sgl.InputActions.Back))
    scope.onDestroy(() => Input.clearInputProcessor())
    context.addSystem(Stage.Update, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.editor-menu")
      override def run(context: GameContext)(using Scope): Unit = {
        services.updateAudio(context.dt, profile.musicVolume * 0.7f, 0f)
        pointer.take().foreach {
          case Act.Key(Keys.Escape) | Act.Back => context.runtime.loadScene(TitleScene)
          case _ => ()
        }
      }
    })
    context.addSystem(Stage.Render, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.editor-menu-render")
      override def run(context: GameContext)(using Scope): Unit = {
        val dt = context.dt
        context.global[Render2D].screen(10) { p =>
          val ui = pointer.ui
          ui.begin(p, services.screen, Layout.scale(services, profile), pointer.controls.mouseX, pointer.controls.mouseY, dt)
          ui.rect(0f, 0f, ui.width, ui.height, Tint(0x2a2028))
          ui.text("Map maker", 30f, 22f, 32f, Tint.Gold, bold = true)
          if(ui.button(ui.width - 130f, 18f, 110f, 40f, "Title")) context.runtime.loadScene(TitleScene)
          ui.panel(30f, 80f, 360f, 300f)
          ui.text("A new map", 54f, 98f, 20f, Tint.Cream, bold = true)
          Vector(64, 96, 128, 176).zipWithIndex.foreach { case (n, i) =>
            if(ui.button(54f + i * 78f, 138f, 72f, 36f, n.toString, enabled = size != i, size = 15f)) size = i
          }
          val n = Vector(64, 96, 128, 176)(size)
          if(ui.button(54f, 192f, 306f, 42f, "Start from a bare meadow")) context.runtime.loadScene(new EditorScene(new HandMap(n), profile.freeMap))
          ui.text(s"Seed $seed", 54f, 252f, 16f, Tint.Soft)
          if(ui.button(230f, 246f, 130f, 34f, "Another", size = 14f)) seed = 1L + (seed * 7919L + 4057L) % 9999L
          if(ui.button(54f, 296f, 306f, 42f, "Start from a valley")) {
            val made = scala.util.Try(MapGen.generate(Rules("Draft", "", n, seed * 977L, Goal.Endless, nests = if(n < 90) 1 else 2))).toOption
            made.foreach(w => context.runtime.loadScene(new EditorScene(HandMap.from(w), profile.freeMap)))
          }
          ui.panel(410f, 80f, ui.width - 440f, ui.height - 110f)
          ui.text("Made before", 434f, 98f, 20f, Tint.Cream, bold = true)
          if(ui.button(ui.width - 200f, 92f, 134f, 34f, "Paste a map", size = 14f)) services.paste(t => Share.readMap(t).foreach(m => profile.keepMap(profile.freeMap, m)))
          if(profile.maps.isEmpty) ui.text("None yet.", 434f, 136f, 15f, Tint.Soft)
          profile.maps.zipWithIndex.foreach { case (m, i) =>
            val y = 136f + i * 46f
            ui.text(if(profile.mapName(m).isEmpty) s"Map $m" else Lang.verbatim(profile.mapName(m)), 434f, y + 9f, 17f, Tint.Cream, bold = true)
            if(ui.button(ui.width - 384f, y, 44f, 38f, "", icon = ui.art.icon("share"))) profile.mapText(m).foreach(h => services.copy(Share.map(h)))
            if(ui.button(ui.width - 330f, y, 120f, 38f, "Edit", size = 15f)) profile.mapText(m).foreach(h => context.runtime.loadScene(new EditorScene(h, m)))
            if(ui.button(ui.width - 200f, y, 90f, 38f, "Play", size = 15f)) profile.mapText(m).foreach(h => Scenes.enter(context, EditorScene.rules(m, h), -1, fresh = true))
            if(ui.button(ui.width - 100f, y, 44f, 38f, "", icon = ui.art.icon("remove"))) profile.dropMap(m)
          }
          ui.end()
        }
      }
    })
  }
}

/** A map being made. */
final class EditorScene(map: HandMap, number: Int) extends Scene {
  private enum Brush { case Paint(floor: Floor); case Ore(item: Item); case NoOre; case Vents; case Build(solid: Solid); case Hearth; case Nest; case NoNest }
  private val T = Valley.Tile.toFloat

  override def load(context: GameContext)(using scope: Scope): Unit = {
    val services = context.global[Services]
    val profile = context.global[Profile]
    val art = context.global[Gallery]
    val rules = EditorScene.rules(number, map).copy(peaceful = true, free = true)
    // A valley to show it with, whose land is that of the map.
    val world = new Valley(map.size, map.size, rules)
    def sync(): Unit = {
      java.lang.System.arraycopy(map.floors, 0, world.floors, 0, map.floors.length)
      java.lang.System.arraycopy(map.ores, 0, world.ores, 0, map.ores.length)
      java.lang.System.arraycopy(map.solids, 0, world.solids, 0, map.solids.length)
    }
    sync()
    val rng = new Rng(map.size * 31L + 7L)
    world.grain.indices.foreach(i => world.grain(i) = (rng.nextInt() & 0x3f).toByte)
    val terrain = new Terrain(world, art, context.engine.renderer)
    val camera = context.spawn2D("camera", Transform2D(Vec2(map.hearthX * T, map.hearthY * T))).attach(new Camera2D(_, Vec2(800f, 450f)))
    val cam = camera.get[Camera2D]
    val pointer = new Pointer(new Ui(art, () => services.play(SoundKind.Click, 0.5f * profile.soundVolume, 1f)), services)
    Input.setInputProcessor(pointer.controls, Set(sgl.InputActions.Back))
    scope.onDestroy(() => Input.clearInputProcessor())

    var cx = map.hearthX * T
    var cy = map.hearthY * T
    var zoom = 1f
    var time = 0f
    var brush: Brush = Brush.Paint(Floor.Loam)
    var radius = 2
    var painting = false
    var panning = false
    var mirror = false
    var rulesOpen = false
    var lastX = 0f
    var lastY = 0f
    var said = ""
    var saidAge = 10f
    // What was there before each stroke, to go back to.
    val undo = mutable.ArrayDeque.empty[(Array[Byte], Array[Byte], Array[Byte], Int, Int, Vector[(Int, Int)])]
    def remember(): Unit = {
      undo.append((map.floors.clone(), map.ores.clone(), map.solids.clone(), map.hearthX, map.hearthY, map.nests.toVector))
      if(undo.size > 30) { val _ = undo.removeHead() }
    }
    def back(): Unit = if(undo.nonEmpty) {
      val (f, o, s, hx, hy, nests) = undo.removeLast()
      java.lang.System.arraycopy(f, 0, map.floors, 0, f.length)
      java.lang.System.arraycopy(o, 0, map.ores, 0, o.length)
      java.lang.System.arraycopy(s, 0, map.solids, 0, s.length)
      map.hearthX = hx
      map.hearthY = hy
      map.nests.clear()
      map.nests ++= nests
      sync()
      for(y <- 0 until map.size by 16; x <- 0 until map.size by 16) terrain.touched(x + 2, y + 2)
    }
    val sheet = new RulesView(map, pointer.ui, () => rulesOpen = false)
    def tell(text: String): Unit = { said = text; saidAge = 0f }
    def worldAt(px: Float, py: Float): Vec2 = cam.screenToWorld(Vec2(px, py), services.screen)

    def put(tx: Int, ty: Int): Unit = if(tx >= 0 && ty >= 0 && tx < map.size && ty < map.size) {
      val i = ty * map.size + tx
      brush match {
        case Brush.Paint(f) => map.floors(i) = f.ordinal.toByte; if(f.water) map.solids(i) = 0
        case Brush.Ore(item) => if(map.solids(i) == 0) map.ores(i) = item.id.toByte
        case Brush.Vents => if(map.solids(i) == 0) map.ores(i) = Valley.Vent
        case Brush.NoOre => map.ores(i) = -1
        case Brush.Build(s) => map.solids(i) = s.ordinal.toByte; if(s != Solid.Open) map.ores(i) = -1
        case _ => ()
      }
    }
    def stroke(px: Float, py: Float): Unit = {
      val at = worldAt(px, py)
      val tx = floor(at.x / T)
      val ty = floor(at.y / T)
      def one(x: Int, y: Int): Unit = brush match {
        case Brush.Hearth =>
          map.hearthX = clamp(x - 1, 2, map.size - 5)
          map.hearthY = clamp(y - 1, 2, map.size - 5)
        case Brush.Nest =>
          if(!map.nests.exists { case (nx, ny) => scala.math.abs(nx - x + 1) < 6 && scala.math.abs(ny - y + 1) < 6 } && x > 3 && y > 3 && x < map.size - 4 && y < map.size - 4) map.nests += ((x - 1, y - 1))
        case Brush.NoNest => map.nests.filterInPlace { case (nx, ny) => scala.math.abs(nx + 1 - x) > 2 || scala.math.abs(ny + 1 - y) > 2 }
        case _ =>
          for(dy <- -radius to radius; dx <- -radius to radius if dx * dx + dy * dy <= radius * radius + radius * 0.5f) {
            put(x + dx, y + dy)
          }
          sync()
          for(dy <- -radius to radius by scala.math.max(1, radius); dx <- -radius to radius by scala.math.max(1, radius)) terrain.touched(clamp(x + dx, 0, map.size - 1), clamp(y + dy, 0, map.size - 1))
      }
      one(tx, ty)
      if(mirror) brush match {
        case Brush.Hearth => ()
        case _ => one(map.size - 1 - tx, map.size - 1 - ty)
      }
    }

    context.addSystem(Stage.Update, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.editor")
      override def run(context: GameContext)(using Scope): Unit = {
        val dt = context.dt
        time += dt
        saidAge += dt
        val c = pointer.controls
        def held(k: Keys.Key) = if(c.isHeld(k) && !rulesOpen) 1f else 0f
        cx += (held(Keys.D) - held(Keys.A) + held(Keys.Right) - held(Keys.Left)) * 900f * dt / zoom
        cy += (held(Keys.S) - held(Keys.W) + held(Keys.Down) - held(Keys.Up)) * 900f * dt / zoom
        c.take().foreach {
          case Act.Press(x, y, b) =>
            lastX = x
            lastY = y
            if(pointer.ui.catches(x, y)) { if(b == 0) pointer.ui.press(x, y) }
            else if(b == 0) { remember(); painting = true; stroke(x, y) }
            else panning = true
          case Act.Drag(x, y) =>
            if(panning) { cx -= (x - lastX) / zoom; cy -= (y - lastY) / zoom }
            if(painting) brush match {
              case Brush.Nest | Brush.NoNest => ()
              case _ => stroke(x, y)
            }
            lastX = x
            lastY = y
          case Act.Release(x, y, b) =>
            if(b == 0) { pointer.ui.release(x, y); painting = false } else panning = false
          case Act.Wheel(n, _, _) => zoom = clamp(zoom * scala.math.pow(1.15, -n.toDouble).toFloat, 0.25f, 3f)
          case Act.Pinch(dx, dy, f, _, _) => cx -= dx / zoom; cy -= dy / zoom; zoom = clamp(zoom * f, 0.25f, 3f)
          case Act.Text(t) => if(rulesOpen) sheet.typed(t)
          case Act.Key(Keys.Escape) | Act.Back if rulesOpen => rulesOpen = false
          case Act.Key(k) if rulesOpen => sheet.key(k)
          case Act.Key(Keys.F11) => services.fullscreen = !services.fullscreen
          case Act.Key(Keys.Z) | Act.Key(Keys.Backspace) => back()
          case Act.Key(Keys.Escape) | Act.Back => context.runtime.loadScene(EditorMenuScene)
          case _ => ()
        }
        val s = services.screen
        cx = clamp(cx, 0f, map.size * T)
        cy = clamp(cy, 0f, map.size * T)
        cam.viewportSize = Vec2(s.x / zoom, s.y / zoom)
        camera.spatial.local = Transform2D(Vec2(scala.math.round(cx * zoom) / zoom, scala.math.round(cy * zoom) / zoom))
        services.updateAudio(dt, profile.musicVolume * 0.6f, 0f)
      }
    })
    context.addSystem(Stage.Render, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.editor-render")
      override def run(context: GameContext)(using Scope): Unit = {
        val dt = context.dt
        val draw = context.global[Render2D]
        val s = services.screen
        val (x0, y0, w, h) = (cx - s.x / zoom / 2f, cy - s.y / zoom / 2f, s.x / zoom, s.y / zoom)
        draw.world(0f) { p =>
          terrain.drawFloors(p, x0, y0, x0 + w, y0 + h)
          terrain.drawRipples(p, x0, y0, x0 + w, y0 + h, time)
        }
        draw.world(2f)(terrain.drawSolids(_, x0, y0, x0 + w, y0 + h))
        draw.world(3f) { p =>
          p.sprite(art.base(Blocks.Hearth.id), (map.hearthX + 1.5f) * T, (map.hearthY + 1.5f) * T, 3 * T, 3 * T)
          p.sprite(art.flames((time * 10f).toInt & 3), (map.hearthX + 1.5f) * T, (map.hearthY + 1.2f) * T, 50f, 50f)
          map.nests.foreach { case (nx, ny) => p.sprite(art.base(Blocks.Nest.id), (nx + 1.5f) * T, (ny + 1.5f) * T, 3 * T, 3 * T) }
          if(!pointer.ui.catches(pointer.controls.mouseX, pointer.controls.mouseY)) {
            val at = worldAt(pointer.controls.mouseX, pointer.controls.mouseY)
            val r = (radius + 0.5f) * T
            p.sprite(art.ring, (floor(at.x / T) + 0.5f) * T, (floor(at.y / T) + 0.5f) * T, r * 2.2f, r * 2.2f, 0f, Color(1f, 1f, 1f, 0.8f))
          }
        }
        draw.screen(10) { p =>
          val ui = pointer.ui
          ui.begin(p, services.screen, Layout.scale(services, profile), pointer.controls.mouseX, pointer.controls.mouseY, dt)
          palette(ui)
          if(rulesOpen) sheet.draw()
          ui.end()
        }
      }

      private def palette(ui: Ui): Unit = {
        ui.panel(8f, 8f, 168f, ui.height - 16f, 0.95f)
        val called = if(map.rules.name.isEmpty) s"Map $number" else Lang.verbatim(map.rules.name)
        ui.text(called, 24f, 20f, ui.fitting(called, 140f, 20f), Tint.Gold, bold = true)
        ui.text(s"${map.size} by ${map.size}, ${map.nests.size} hearts", 24f, 46f, 12f, Tint.Soft)
        var y = 68f
        def title(t: String): Unit = { ui.text(t, 24f, y, 13f, Tint.Soft, bold = true); y += 20f }
        def grid(entries: Vector[(Brush, (Float, Float, Float) => Unit, String)]): Unit = {
          entries.zipWithIndex.foreach { case ((b, paint, label), i) =>
            val bx = 22f + (i % 4) * 36f
            val by = y + (i / 4) * 36f
            if(ui.slot(bx, by, 33f, brush == b)) brush = b
            paint(bx + 4f, by + 4f, 25f)
            if(ui.over(bx, by, 33f, 33f)) {
              val w = ui.textWidth(label, 13f) + 22f
              ui.paper(184f, by, w, 30f)
              ui.text(label, 195f, by + 7f, 13f, Tint.Ink)
            }
          }
          y += ((entries.size + 3) / 4) * 36f + 8f
        }
        val spring = Palette.all(0)
        def swatch(c: Color)(x: Float, y: Float, s: Float): Unit = ui.rect(x, y, s, s, c)
        title("Ground")
        grid(Floor.all.toVector.map(f => (Brush.Paint(f), swatch(if(f == Floor.Deep) spring.deep else if(f == Floor.Shallow) spring.shallow else spring.floors(f.ordinal)), f.toString)))
        title("On it")
        grid(Vector(
          (Brush.Build(Solid.Cliff), (x, y, s) => ui.picture(art.cliffs(0), x, y, s, s), "Cliff"),
          (Brush.Build(Solid.Thicket), (x, y, s) => ui.picture(art.trees(0), x, y, s, s, Tint(0x8fc255)), "Thicket"),
          (Brush.Build(Solid.Open), (x, y, s) => ui.picture(art.icon("remove"), x, y, s, s), "Clear")
        ))
        title("Seams")
        grid(Vector(Items.Copper, Items.Tin, Items.Coal, Items.Flint, Items.Iron).map(i => (Brush.Ore(i), (x: Float, y: Float, s: Float) => ui.picture(art.items(i.id), x, y, s, s), i.label)) ++ Vector(
          (Brush.Vents, (x: Float, y: Float, s: Float) => ui.picture(art.vent, x, y, s, s), "Vent"),
          (Brush.NoOre, (x: Float, y: Float, s: Float) => ui.picture(art.icon("remove"), x, y, s, s), "Clear")
        ))
        title("Hearths")
        grid(Vector(
          (Brush.Hearth, (x, y, s) => ui.picture(art.base(Blocks.Hearth.id), x, y, s, s), "The Hearth"),
          (Brush.Nest, (x, y, s) => ui.picture(art.base(Blocks.Nest.id), x, y, s, s), "Briar Heart"),
          (Brush.NoNest, (x, y, s) => ui.picture(art.icon("remove"), x, y, s, s), "Take one away")
        ))
        title("Brush")
        Vector(0, 1, 2, 4, 7).zipWithIndex.foreach { case (r, i) => if(ui.button(22f + i * 28f, y, 26f, 26f, (r + 1).toString, enabled = radius != r, size = 13f)) radius = r }
        y += 34f
        mirror = ui.toggle(22f, y, 140f, "Mirrored", mirror)
        y += 36f
        if(ui.button(22f, y, 140f, 32f, "Undo (Z)", undo.nonEmpty, 14f)) back()
        if(ui.button(22f, ui.height - 145f, 140f, 30f, "Rules", size = 14f)) rulesOpen = true
        val by = ui.height - 112f
        if(ui.button(22f, by, 140f, 30f, "Save", size = 14f)) { profile.keepMap(number, map); tell("Saved") }
        if(ui.button(22f, by + 33f, 140f, 30f, "Play it", size = 14f)) {
          if(map.nests.isEmpty) tell("It has no Briar Heart yet")
          profile.keepMap(number, map)
          if(map.nests.nonEmpty) Scenes.enter(context, EditorScene.rules(number, map), -1, fresh = true)
        }
        if(ui.button(22f, by + 66f, 140f, 30f, "Leave", size = 14f)) context.runtime.loadScene(EditorMenuScene)
        if(saidAge < 2f) ui.centered(said, ui.width / 2f, 30f, 22f, Tint.Cream.withAlpha(scala.math.min(1f, 2f - saidAge)), bold = true)
      }
    })
  }
}
object EditorScene {
  def rules(number: Int, map: HandMap): Rules =
    map.rules.copy(name = if(map.rules.name.isEmpty) s"Map $number" else s"${map.rules.name} ($number)", seed = number * 97L, nests = map.nests.size, map = number)
}
