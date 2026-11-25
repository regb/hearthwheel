package hearthwheel
package core

import scala.util.{Failure, Success}

import sgl.Input
import sgl.Input.Keys
import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d._
import hearthwheel.model._
import hearthwheel.model.MathUtil._

object Scenes {
  def startOver(context: GameContext, play: Play): Unit = {
    val profile = context.global[Profile]
    context.runtime.loadScene(new PlayScene(() => fresh(profile, play.world.rules), play.valley))
  }

  def fresh(profile: Profile, rules: Rules): Valley =
    if(rules.map > 0) profile.mapText(rules.map).map(_.build(rules)).getOrElse(MapGen.generate(rules.copy(map = 0)))
    else MapGen.generate(rules)

  /** Goes on with a valley that is under way, or makes it. */
  def enter(context: GameContext, rules: Rules, valley: Int, fresh: Boolean): Unit = {
    val profile = context.global[Profile]
    val saved = if(fresh) None else profile.savedWorld(rules.name)
    context.runtime.loadScene(new PlayScene(() => saved.map(Codec.load(_, rules)) match {
      case Some(Success(world)) => world
      case Some(Failure(_)) | None => Scenes.fresh(profile, rules)
    }, valley))
  }
}

/** A pointer and keys for the screens outside of a valley. */
private final class Pointer(val ui: Ui, services: Services) {
  val controls = new Controls
  def take(): Vector[Act] = {
    val acts = controls.take()
    acts.foreach {
      case Act.Press(x, y, 0) => ui.press(x, y)
      case Act.Release(x, y, 0) => ui.release(x, y)
      case Act.Wheel(n, _, _) => ui.wheel(n)
      case Act.Key(Keys.F11) => services.fullscreen = !services.fullscreen
      case _ => ()
    }
    acts
  }
}

/** The title, over a valley that minds itself while the year goes by. */
object TitleScene extends Scene {
  private final class TitleState(val pointer: Pointer) { var sheet = 0; var time = 0f; var binding: Option[Command] = None; var codex: Block = null }

  private val rules = Rules("Title", "", 96, 2024L, Goal.Endless, nests = 2, threat = 0.9f, seasons = Vector(26f, 26f, 26f, 26f), firstWave = 4f, waveSeconds = 22f, water = 0.6f, free = true)
  private object backdrop extends PlayScene(() => Showcase.build(MapGen.generate(rules)), -1) {
    override protected def attract: Boolean = true
    override protected def addInterface(context: GameContext)(using Scope): Unit = ()
  }

  override def load(context: GameContext)(using scope: Scope): Unit = {
    backdrop.load(context)
    val services = context.global[Services]
    val profile = context.global[Profile]
    val play = context.global[Play]
    val state = new TitleState(new Pointer(new Ui(context.global[Gallery], () => services.play(SoundKind.Click, 0.5f * profile.soundVolume, 1f)), services))
    context.setGlobal(state)
    Input.setInputProcessor(state.pointer.controls)
    context.addSystem(Stage.Update, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.title")
      override def run(context: GameContext)(using Scope): Unit = {
        state.time += context.dt
        // The camera wanders around the Hearth.
        play.following = false
        play.zoomGoal = 1.35f
        play.centerX = play.world.hearth.cx + sin(state.time * 0.07f) * 260f
        play.centerY = play.world.hearth.cy + cos(state.time * 0.053f) * 200f
        state.pointer.take().foreach {
          case Act.Key(key) if state.binding.nonEmpty =>
            if(key != Keys.Escape && Bindings.all.contains(key)) profile.bindings.bind(state.binding.get, key)
            state.binding = None
          case Act.Key(Keys.Escape) => state.sheet = 0
          case Act.Key(Keys.Enter | Keys.NumpadEnter) => context.runtime.loadScene(ValleyScene)
          case _ => ()
        }
      }
    })
    context.addSystem(Stage.Render, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.title-render")
      override def run(context: GameContext)(using Scope): Unit = {
        val dt = context.dt
        context.global[Render2D].screen(10) { p =>
          val ui = state.pointer.ui
          ui.begin(p, services.screen, Layout.scale(services, profile), state.pointer.controls.mouseX, state.pointer.controls.mouseY, dt)
          draw(context, ui, state, profile, services, play)
          ui.end()
        }
      }
    })
  }

  private def draw(context: GameContext, ui: Ui, state: TitleState, profile: Profile, services: Services, play: Play): Unit = {
    ui.picture(ui.art("vignette"), 0f, 0f, ui.width, ui.height, Color(0f, 0f, 0f, 0.75f))
    val w = scala.math.min(680f, ui.width - 40f)
    val h = w * 190f / 900f
    ui.picture(ui.art("title"), ui.width / 2f - w / 2f, ui.height * 0.07f + sin(state.time * 1.1f) * 4f, w, h)
    val line = "Build, guard and wind the year"
    ui.outlined(line, ui.width / 2f - ui.textWidth(line, 19f, bold = true) / 2f, ui.height * 0.07f + h + 6f, 19f, Tint.Cream)
    val s = play.world.season
    ui.picture(ui.art.seasons(s.ordinal), ui.width / 2f - 22f, ui.height * 0.07f + h + 34f, 44f, 44f, Tint.alpha(0.95f), sin(state.time) * 0.12f)
    val bx = ui.width / 2f - 120f
    val by = ui.height * 0.07f + h + 88f
    if(ui.button(bx, by, 240f, 48f, "Play", size = 20f)) context.runtime.loadScene(ValleyScene)
    if(ui.button(bx, by + 55f, 240f, 42f, "Saved games")) state.sheet = 1
    if(ui.button(bx, by + 103f, 240f, 42f, "Map maker")) context.runtime.loadScene(EditorMenuScene)
    if(ui.button(bx, by + 151f, 240f, 42f, "Codex")) state.sheet = 2
    if(ui.button(bx, by + 199f, 240f, 42f, "Settings")) state.sheet = 3
    if(ui.button(bx, by + 247f, 240f, 42f, "Quit")) services.exit()
    ui.right("An example of SGL, the Scala Game Library", ui.width - 14f, ui.height - 26f, 13f, Tint.Cream.withAlpha(0.8f))
    state.sheet match {
      case 1 => SavesView.draw(ui, profile, None, context, () => state.sheet = 0)
      case 2 => new CodexView(play.world, profile, ui, state.codex, b => state.codex = b, () => state.sheet = 0).draw()
      case 3 => Settings.draw(ui, profile, () => state.sheet = 0, () => state.sheet = 4)
      case 4 => Settings.keys(ui, profile, state.binding, c => state.binding = c, () => state.sheet = 3)
      case _ => ()
    }
  }

  override def unload(context: GameContext)(using Scope): Unit = context.global[Profile].save()
}

/** The region: its valleys, what may be brought to them, and one's own. */
object ValleyScene extends Scene {
  private final class Custom {
    var seed = 1L; var size = 1; var goal = 0; var threat = 1; var nests = 2; var season = 0; var length = 1; var start = 0
    var free = false; var peaceful = false; var calm = false; var patient = false; var timer = true; var wild = true
    var water = 0.5f; var cliffs = 0.5f; var thickets = 0.5f; var ore = 1f
    var map = 0
  }
  private var selected = 0
  private var own = false
  /** What is brought along, by item, and whether it is being chosen. */
  private val load = new Array[Int](Items.Count)
  private var packing = false
  private val custom = new Custom
  private val LoadLimit = 600

  override def load(context: GameContext)(using scope: Scope): Unit = {
    val services = context.global[Services]
    val profile = context.global[Profile]
    val art = context.global[Gallery]
    val pointer = new Pointer(new Ui(art, () => services.play(SoundKind.Click, 0.5f * profile.soundVolume, 1f)), services)
    custom.seed = 1L + scala.math.abs(services.seed() % 9999L)
    selected = Valleys.all.indices.filter(profile.open).maxByOption(i => if(profile.won.contains(i)) -1 else i).getOrElse(0)
    packing = false
    Input.setInputProcessor(pointer.controls, Set(sgl.InputActions.Back))
    scope.onDestroy(() => Input.clearInputProcessor())
    val previews = scala.collection.mutable.HashMap.empty[String, Baked2D]
    context.addSystem(Stage.Update, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.valleys")
      override def run(context: GameContext)(using Scope): Unit = {
        services.music(2)
        services.air(-1)
        services.updateAudio(context.dt, profile.musicVolume * 0.7f, profile.airVolume)
        pointer.take().foreach {
          case Act.Key(Keys.Escape) | Act.Back => if(packing) packing = false else context.runtime.loadScene(TitleScene)
          case _ => ()
        }
      }
    })
    context.addSystem(Stage.Render, new GameSystem {
      override val id: SystemId = SystemId("hearthwheel.valleys-render")
      override def run(context: GameContext)(using Scope): Unit = {
        val dt = context.dt
        context.global[Render2D].screen(10) { p =>
          val ui = pointer.ui
          ui.begin(p, services.screen, Layout.scale(services, profile), pointer.controls.mouseX, pointer.controls.mouseY, dt)
          // Made when first looked at: a valley takes a moment.
          def preview(rules: Rules): (Baked2D, Int) = {
            val key = s"${rules.seed}/${rules.size}/${rules.map}/${rules.water}/${rules.cliffs}/${rules.thickets}/${rules.ore}/${rules.nests}"
            var size = rules.size
            val baked = previews.getOrElseUpdate(key, {
              // Not every wish can be granted: one that cannot shows nothing.
              val world = scala.util.Try(Scenes.fresh(profile, rules)).toOption
              context.engine.renderer.bake2D(p => world.foreach(Minimap.paint(_, p)))(using scope)
            })
            if(rules.map > 0) profile.mapText(rules.map).foreach(m => size = m.size)
            (baked, size)
          }
          draw(context, ui, profile, preview)
          ui.end()
        }
      }
    })
  }

  private def draw(context: GameContext, ui: Ui, profile: Profile, preview: Rules => (Baked2D, Int)): Unit = {
    ui.rect(0f, 0f, ui.width, ui.height, Tint(0x2a2028))
    val mapW = ui.width - 440f
    region(ui, profile, 20f, 76f, mapW - 20f, ui.height - 96f)
    ui.text(if(own) "A valley of one's own" else "The region", 30f, 22f, 32f, Tint.Gold, bold = true)
    if(ui.button(ui.width - 130f, 18f, 110f, 40f, "Title")) context.runtime.loadScene(TitleScene)
    if(ui.button(ui.width - 330f, 18f, 190f, 40f, if(own) "The region" else "One's own valley")) own = !own
    val x = mapW + 14f
    val w = ui.width - x - 20f
    val h = ui.height - 96f
    if(own) {
      ui.panel(20f, 76f, ui.width - 40f, h)
      ownValley(context, ui, profile, 20f, 76f, ui.width - 40f, h, preview)
    } else {
      ui.panel(x, 76f, w, h)
      valley(context, ui, profile, selected, x, 76f, w, h, preview)
    }
    if(packing) pack(context, ui, profile)
  }

  // ------------------------------------------------------------ the region

  private def region(ui: Ui, profile: Profile, x: Float, y: Float, w: Float, h: Float): Unit = {
    ui.paper(x, y, w, h)
    def px(i: Int) = x + 30f + Valleys.places(i).x / 100f * (w - 60f)
    def py(i: Int) = y + 30f + Valleys.places(i).y / 100f * (h - 60f)
    // Hills and woods, drawn by hand, always the same.
    ui.clip(x + 8f, y + 8f, w - 16f, h - 16f) {
      val rng = new Rng(77L)
      for(_ <- 0 until 130) {
        val hx = x + rng.float() * w
        val hy = y + rng.float() * h
        val near = Valleys.places.indices.exists(i => MathUtil.dist(hx, hy, px(i), py(i)) < 44f || (scala.math.abs(hx - px(i)) < 78f && hy > py(i) && hy < py(i) + 60f))
        if(!near) {
          if(rng.chance(0.55f)) ui.picture(ui.art.cliffs(rng.int(4)), hx - 14f, hy - 14f, 28f, 28f, Color(0.78f, 0.66f, 0.5f, 0.5f))
          else ui.picture(ui.art.trees(rng.int(3)), hx - 11f, hy - 11f, 22f, 22f, Color(0.68f, 0.66f, 0.42f, 0.5f))
        }
      }
    }
    Valleys.places.indices.foreach { i =>
      Valleys.places(i).after.foreach { from =>
        val done = profile.won.contains(from)
        val n = 14
        for(k <- 1 until n) {
          val t = k.toFloat / n
          val bend = sin(t * Pi) * 10f
          ui.picture(ui.art.mote, lerp(px(from), px(i), t) - 4f, lerp(py(from), py(i), t) - bend - 4f, 8f, 8f, if(done) Tint(0x8c3a24) else Tint(0x8c3a24, 0.28f))
        }
      }
    }
    Valleys.places.indices.foreach { i =>
      val r = Valleys.places(i).rules
      val open = profile.open(i)
      val won = profile.won.contains(i)
      val s = if(selected == i) 50f + 2f * sin(ui.time * 4f) else 44f
      val bx = px(i) - s / 2f
      val by = py(i) - s / 2f
      ui.catchMouse(bx, by, s, s)
      if(selected == i) ui.additive(ui.picture(ui.art.glow, px(i) - 50f, py(i) - 50f, 100f, 100f, Color(1f, 0.6f, 0.3f, 0.6f)))
      ui.picture(ui.art.base(Blocks.Hearth.id), bx, by, s, s, if(open) Color.White else Color(0.45f, 0.42f, 0.44f, 0.9f))
      if(won) ui.picture(ui.art.flames((ui.time * 10f).toInt & 3), px(i) - 13f, py(i) - 18f, 26f, 26f)
      else if(!open) ui.picture(ui.art.icon("lock"), px(i) - 11f, py(i) - 11f, 22f, 22f)
      else ui.picture(ui.art.seasons(r.startSeason), px(i) - 12f, py(i) - 12f, 24f, 24f)
      ui.centered(r.name, px(i), by + s + 2f, 14f, if(open) Sheets.Brown else Tint(0x9c8a78), bold = true)
      if(profile.savedWorld(r.name).nonEmpty) ui.picture(ui.art.icon("pin"), bx + s - 14f, by - 10f, 22f, 22f)
      if(ui.clicked(bx, by, s, s)) selected = i
    }
  }

  private def valley(context: GameContext, ui: Ui, profile: Profile, i: Int, x: Float, y: Float, w: Float, h: Float, preview: Rules => (Baked2D, Int)): Unit = {
    val place = Valleys.places(i)
    val r = place.rules
    val tw = w - 48f
    ui.text(r.name, x + 24f, y + 20f, 27f, Tint.Gold, bold = true)
    var at = ui.wrapped(r.about, x + 24f, y + 58f, tw, 14f, Tint.Cream) + 8f
    at = ui.wrapped(r.goal.label, x + 24f, at, tw, 14f, Tint.Good, bold = true) + 8f
    r.seasons.zipWithIndex.foreach { case (s, k) =>
      ui.picture(ui.art.seasons(k), x + 24f + k * 72f, at, 22f, 22f)
      ui.text(Layout.time(s), x + 49f + k * 72f, at + 3f, 13f)
    }
    at += 32f
    ui.text("Threat", x + 24f, at + 2f, 13f, Tint.Soft, bold = true)
    for(k <- 0 until 5) ui.picture(ui.art.icon("wave"), x + 82f + k * 24f, at - 2f, 20f, 20f, if(k < scala.math.round(r.threat * profile.difficulty.threat * 3.2f)) Color.White else Tint.alpha(0.2f))
    ui.text(profile.difficulty.label, x + 212f, at + 2f, 13f, Tint.Soft)
    at += 28f
    val map = scala.math.min(tw, y + h - at - 120f)
    if(map > 60f) {
      val (baked, size) = preview(r)
      val mx = x + (w - map) / 2f
      ui.rect(mx - 3f, at - 3f, map + 6f, map + 6f, Tint(0xb0784a))
      ui.raw.baked(baked, mx * ui.scale, at * ui.scale, map / size * ui.scale)
    }
    val by = y + h - 60f
    val open = profile.open(i)
    val saved = profile.savedWorld(r.name).nonEmpty
    if(!open) {
      val missing = place.needs.filterNot(profile.learned.contains).map(_.label)
      val before = if(place.after.isEmpty || place.after.exists(profile.won.contains)) "" else "Win " + place.after.map(k => Valleys.all(k).name).mkString(" or ")
      val text = Vector(before, if(missing.isEmpty) "" else "learn " + missing.mkString(", ")).filter(_.nonEmpty).mkString(", and ")
      val _ = ui.wrapped(text, x + 24f, by - 10f, tw, 14f, Tint.Bad)
    } else if(saved) {
      if(ui.button(x + 24f, by, (tw - 10f) / 2f, 44f, "Go on", size = 17f)) Scenes.enter(context, r, i, fresh = false)
      if(ui.button(x + 34f + (tw - 10f) / 2f, by, (tw - 10f) / 2f, 44f, "Start over", size = 15f)) begin(profile, i, context)
    } else if(ui.button(x + 24f, by, tw, 44f, "Enter the valley", size = 17f)) begin(profile, i, context)
  }

  // With something in the store outside, what to bring is chosen first.
  private def begin(profile: Profile, i: Int, context: GameContext): Unit = {
    selected = i
    java.util.Arrays.fill(load, 0)
    if(profile.vault.exists(_ > 0)) packing = true else enter(context, profile)
  }
  private def enter(context: GameContext, profile: Profile): Unit = {
    val r = Valleys.places(selected).rules
    val along = Items.all.filter(i => load(i.id) > 0).map(i => Stack(i, load(i.id)))
    along.foreach(s => profile.vault(s.item.id) -= s.amount)
    profile.save()
    packing = false
    Scenes.enter(context, r.copy(threat = r.threat * profile.difficulty.threat, toughness = r.toughness * profile.difficulty.toughness, start = r.start ++ along), selected, fresh = true)
  }

  private def pack(context: GameContext, ui: Ui, profile: Profile): Unit = {
    val w = 620f
    val h = 470f
    val (x, y) = Sheets.open(ui, "What to bring", w, h, () => packing = false)
    val used = load.sum
    ui.text(s"$used of $LoadLimit", x + 220f, y + 22f, 15f, if(used >= LoadLimit) Tint(0xc0402c) else Sheets.Dim, bold = true)
    Items.all.foreach { i =>
      val bx = x + 28f + (i.id % 2) * 290f
      val by = y + 60f + (i.id / 2) * 46f
      ui.picture(ui.art.items(i.id), bx, by + 4f, 28f, 28f)
      ui.text(i.label, bx + 36f, by, 14f, Tint.Ink, bold = true)
      ui.text(s"${Layout.amount(profile.vault(i.id))} in the store", bx + 36f, by + 19f, 12f, Sheets.Dim)
      if(ui.button(bx + 156f, by + 2f, 28f, 30f, "-", load(i.id) > 0, 16f)) load(i.id) = scala.math.max(0, load(i.id) - 25)
      ui.centered(load(i.id).toString, bx + 208f, by + 8f, 15f, Tint.Ink, bold = true)
      if(ui.button(bx + 232f, by + 2f, 28f, 30f, "+", load(i.id) < profile.vault(i.id) && used < LoadLimit, 16f))
        load(i.id) = scala.math.min(scala.math.min(profile.vault(i.id), load(i.id) + 25), load(i.id) + LoadLimit - used)
    }
    if(ui.button(x + w / 2f - 110f, y + h - 60f, 220f, 44f, "Enter the valley", size = 17f)) enter(context, profile)
  }

  // ------------------------------------------------------ a valley of one's own

  private val sizes = Vector("Small" -> 80, "Middling" -> 112, "Wide" -> 144, "Vast" -> 192)
  private val threats = Vector("Mild" -> 0.6f, "Even" -> 1f, "Harsh" -> 1.5f, "Cruel" -> 2.2f)
  private val goals = Vector("Endless" -> Goal.Endless, "Two years" -> Goal.Survive(2), "Cleanse" -> Goal.Cleanse, "Great Wheel" -> Goal.Wheel)
  private val lengths = Vector("Quick" -> 0.5f, "Usual" -> 1f, "Long" -> 1.8f)
  private val starts = Vector("Poor" -> 1, "Fair" -> 3, "Rich" -> 10)

  private def ownValley(context: GameContext, ui: Ui, profile: Profile, x: Float, y: Float, w: Float, h: Float, preview: Rules => (Baked2D, Int)): Unit = {
    val half = w * 0.56f
    def choice(row: Int, label: String, options: Vector[String], now: Int): Int = {
      val cy = y + 22f + row * 44f
      ui.text(label, x + 28f, cy + 7f, 15f, Tint.Soft, bold = true)
      var result = now
      val bw = scala.math.min(104f, (half - 150f) / options.size - 5f)
      options.zipWithIndex.foreach { case (o, i) => if(ui.button(x + 130f + i * (bw + 5f), cy, bw, 34f, o, enabled = i != now, size = 13f)) result = i }
      result
    }
    custom.size = choice(0, "Size", sizes.map(_._1), custom.size)
    custom.goal = choice(1, "Goal", goals.map(_._1), custom.goal)
    custom.threat = choice(2, "Threat", threats.map(_._1), custom.threat)
    custom.length = choice(3, "Seasons", lengths.map(_._1), custom.length)
    custom.season = choice(4, "Starts in", Season.values.map(_.label).toVector, custom.season)
    custom.start = choice(5, "Start", starts.map(_._1), custom.start)
    custom.nests = choice(6, "Hearts", Vector("1", "2", "3", "4", "5"), custom.nests - 1) + 1
    val sy = y + 338f
    custom.water = ui.slider(x + 28f, sy, half / 2f - 40f, "Water", custom.water)
    custom.cliffs = ui.slider(x + 10f + half / 2f, sy, half / 2f - 40f, "Cliffs", custom.cliffs)
    custom.thickets = ui.slider(x + 28f, sy + 48f, half / 2f - 40f, "Thickets", custom.thickets)
    custom.ore = ui.slider(x + 10f + half / 2f, sy + 48f, half / 2f - 40f, "Seams", custom.ore / 2f) * 2f
    val ty = sy + 104f
    custom.free = ui.toggle(x + 28f, ty, half / 2f - 40f, "All known and free", custom.free)
    custom.peaceful = ui.toggle(x + 10f + half / 2f, ty, half / 2f - 40f, "The Bramble sleeps", custom.peaceful)
    custom.calm = ui.toggle(x + 28f, ty + 30f, half / 2f - 40f, "Calm skies", custom.calm)
    custom.patient = ui.toggle(x + 10f + half / 2f, ty + 30f, half / 2f - 40f, "Waves wait for the last foe", custom.patient)
    custom.timer = !ui.toggle(x + 28f, ty + 60f, half / 2f - 40f, "Waves only on call", !custom.timer)
    custom.wild = ui.toggle(x + 10f + half / 2f, ty + 60f, half / 2f - 40f, "The Bramble spreads", custom.wild)
    val k = lengths(custom.length)._2
    val n = starts(custom.start)._2
    val rules = Rules(
      if(custom.map > 0) s"Map ${custom.map}" else s"Valley ${custom.seed}", "", sizes(custom.size)._2, custom.seed * 977L, goals(custom.goal)._2,
      nests = custom.nests, threat = threats(custom.threat)._2, free = custom.free, peaceful = custom.peaceful, calm = custom.calm, patient = custom.patient, timer = custom.timer, wild = custom.wild,
      firstWave = 120f, seasons = Vector(150f, 140f, 140f, 120f).map(_ * k), startSeason = custom.season,
      water = custom.water, cliffs = custom.cliffs, thickets = custom.thickets, ore = custom.ore,
      start = Vector(Stack(Items.Copper, 220 * n), Stack(Items.Tin, 60 * n), Stack(Items.Flint, 40 * n), Stack(Items.Coal, 40 * n)), map = custom.map
    )
    val rx = x + half + 10f
    val rw = w - half - 34f
    val map = scala.math.min(rw, h - 150f)
    ui.text(rules.name, rx, y + 18f, 20f, Tint.Gold, bold = true)
    if(ui.button(rx + rw - 110f, y + 14f, 110f, 34f, "Another", custom.map == 0, 14f)) custom.seed = 1L + (custom.seed * 7919L + 4057L) % 9999L
    if(profile.maps.nonEmpty && ui.button(rx + rw - 240f, y + 14f, 124f, 34f, if(custom.map > 0) "Next map" else "My maps", size = 14f)) {
      val i = profile.maps.indexOf(custom.map)
      custom.map = if(i + 1 < profile.maps.size) profile.maps(i + 1) else 0
    }
    val (baked, size) = preview(rules)
    ui.rect(rx - 3f, y + 57f, map + 6f, map + 6f, Tint(0xb0784a))
    ui.raw.baked(baked, rx * ui.scale, (y + 60f) * ui.scale, map / size * ui.scale)
    val by = y + h - 64f
    val saved = profile.savedWorld(rules.name).nonEmpty
    if(ui.button(rx, by, if(saved) rw / 2f - 5f else rw, 46f, "Enter the valley", size = 17f)) Scenes.enter(context, rules, -1, fresh = true)
    if(saved && ui.button(rx + rw / 2f + 5f, by, rw / 2f - 5f, 46f, "Go on", size = 17f)) Scenes.enter(context, rules, -1, fresh = false)
  }
}
