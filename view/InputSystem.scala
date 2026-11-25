package hearthwheel
package core

import sgl.Input.Keys
import sgl.engine.gameobject._
import hearthwheel.model._
import hearthwheel.model.MathUtil._

/** Turns what the player does into plans, orders and looks around. */
object InputSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.input")
  private val T = Valley.Tile.toFloat

  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    play.controls.take().foreach {
      case Act.Press(x, y, button) =>
        play.lastX = x
        play.lastY = y
        if(play.ui.catches(x, y)) { if(button == 0) play.ui.press(x, y) }
        else press(play, x, y, button)
      case Act.Drag(x, y) =>
        drag(play, x, y)
        play.lastX = x
        play.lastY = y
      case Act.Release(x, y, button) =>
        if(button == 0) play.ui.release(x, y)
        release(play, x, button)
      case Act.Wheel(notches, x, y) =>
        if(play.ui.catches(x, y)) play.ui.wheel(notches)
        else play.zoomAt(scala.math.pow(1.15, -notches.toDouble).toFloat, x, y)
      case Act.Pinch(dx, dy, factor, x, y) =>
        play.following = false
        play.centerX -= dx / play.zoom
        play.centerY -= dy / play.zoom
        play.zoomAt(factor, x, y)
      case Act.Key(key) => this.key(play, key)
      case Act.KeyUp(_) | Act.Text(_) => ()
      case Act.Back => escape(play)
    }
    if(play.shooting && (play.overlay != Overlay.None || play.tool != Tool.Hand)) play.shooting = false
  }

  // --------------------------------------------------------------- pointer

  private def press(play: Play, x: Float, y: Float, button: Int): Unit = {
    play.pressX = x
    play.pressY = y
    play.dragPixels = 0f
    play.dragButton = button
    button match {
      case 2 => play.panning = true
      case 1 =>
        play.dragStart = play.pointerTile
      case _ =>
        play.tool match {
          case Tool.Hand =>
            val (tx, ty) = play.pointerTile
            val b = play.world.buildingAt(tx, ty)
            play.dragStart = (tx, ty)
            if(b == null && (play.riding != null || play.world.lantern != null)) {
              val tile = if(play.world.inside(tx, ty)) play.world.tileIndex(tx, ty) else -1
              // A seam close by is dug; anything else is shot at.
              if(play.riding == null && tile >= 0 && play.world.canMine(tile) && !play.controls.touch) {
                play.world.mining = if(play.world.mining == tile) -1 else tile
                play.sound(SoundKind.Click, 0.4f)
              } else if(!play.controls.touch) play.shooting = true
            }
          case _ => play.dragStart = play.pointerTile
        }
    }
  }

  private def drag(play: Play, x: Float, y: Float): Unit = {
    val dx = x - play.lastX
    val dy = y - play.lastY
    if(play.dragStart != null) {
      play.dragPixels += scala.math.abs(dx) + scala.math.abs(dy)
      val fx = scala.math.abs(x - play.pressX)
      val fy = scala.math.abs(y - play.pressY)
      val (sx, sy) = play.dragStart
      val (tx, ty) = play.pointerTile
      // The way a line bends is settled while it is still straight.
      if(sx == tx || sy == ty) play.horizontalFirst = fx >= fy
      if(play.controls.touch && play.tool == Tool.Hand && play.dragPixels > 12f) play.panning = true
    }
    if(play.panning) {
      play.following = false
      play.centerX -= dx / play.zoom
      play.centerY -= dy / play.zoom
    }
  }

  private def release(play: Play, x: Float, button: Int): Unit = {
    val panned = play.panning && play.dragPixels > 12f
    if(button == 2 || play.controls.touch) play.panning = false
    if(button == 0) play.shooting = false
    val start = play.dragStart
    if(start != null && button == play.dragButton) {
      play.dragStart = null
      // A pinch puts the release far away: nothing was meant.
      if(x > -5000f && !panned) {
        val end = play.pointerTile
        if(button == 1) rightRelease(play, start, end)
        else play.tool match {
          case Tool.Build(block) => placeDragged(play, block, start, end)
          case Tool.Remove => removeBox(play, start, end)
          case Tool.Copy => copyBox(play, start, end)
          case Tool.Stamp(plans) => stamp(play, plans, end)
          case Tool.Command => commandRelease(play, start, end)
          case Tool.Rebuild => rebuildBox(play, start, end)
          case Tool.Hand => if(play.dragPixels < 14f) tap(play, end._1, end._2)
        }
      }
    }
  }

  private def rightRelease(play: Play, start: (Int, Int), end: (Int, Int)): Unit =
    if(play.tool == Tool.Command) order(play, queue = play.controls.shift)
    else if(start != end) removeBox(play, start, end)
    else if(play.tool != Tool.Hand) { play.tool = Tool.Hand; play.lastZipline = null }
    else play.selected = null

  private def tap(play: Play, tx: Int, ty: Int): Unit = {
    val b = play.world.buildingAt(tx, ty)
    val handled = (play.selected, b) match {
      case (p: PylonBuilding, other) if other != null && (other ne p) && other.team == Valley.Player && other.isPowered =>
        if(p.toggle(other)) play.sound(SoundKind.Place)
        else { play.say(if(p.inReach(other)) "No links left" else "Too far", Tint.Bad); play.sound(SoundKind.Deny) }
        true
      case (z: ZiplineBuilding, other: ZiplineBuilding) if other ne z =>
        if(ZiplineBuilding.inReach(z.x, z.y, other.x, other.y, z.reach)) {
          z.configure(if(z.destination eq other) -1 else play.world.tileIndex(other.x, other.y))
          play.sound(SoundKind.Place)
        } else { play.say("Too far, or not in line", Tint.Bad); play.sound(SoundKind.Deny) }
        true
      case (c: CulvertBuilding, other: CulvertBuilding) if other ne c =>
        if(ZiplineBuilding.inReach(c.x, c.y, other.x, other.y, c.reach)) {
          c.configure(if(c.destination eq other) -1 else play.world.tileIndex(other.x, other.y))
          play.sound(SoundKind.Place)
        } else { play.say("Too far, or not in line", Tint.Bad); play.sound(SoundKind.Deny) }
        true
      case (c: CatapultBuilding, other: CatapultBuilding) if other ne c =>
        if(c.inReach(other)) {
          c.configure(if(c.destination eq other) -1 else play.world.tileIndex(other.x, other.y))
          play.sound(SoundKind.Place)
        } else { play.say("Too far", Tint.Bad); play.sound(SoundKind.Deny) }
        true
      case (loom: LoomBuilding, other) if other != null && (other ne loom) && other.team == Valley.Player && !other.isInstanceOf[HearthBuilding] =>
        if(loom.toggle(other)) play.sound(SoundKind.Place)
        else { play.say(if(loom.inReach(other)) "No links left" else "Too far", Tint.Bad); play.sound(SoundKind.Deny) }
        true
      case (loom: LoomBuilding, other) if other eq loom =>
        play.overlay = Overlay.Loom
        true
      case (_, sign: SignBuilding) if play.selected eq sign =>
        sign.configure(sign.config + 1)
        play.sound(SoundKind.Click)
        true
      case (_, s: SwitchBuilding) =>
        s.configure(1 - s.config)
        play.sound(SoundKind.Click)
        true
      case _ =>
        // What is carried goes to whatever takes it.
        val ground = play.world.inside(tx, ty) && b == null
        if(ground && play.controls.touch && play.world.canMine(play.world.tileIndex(tx, ty))) {
          play.world.mining = play.world.tileIndex(tx, ty)
          true
        } else b != null && b.team == Valley.Player && play.world.cargoAmount > 0 && play.world.handOver(b) > 0
    }
    if(!handled) {
      play.selected = if(b == null || (b eq play.selected)) null else b
      if(b != null) play.sound(SoundKind.Click, 0.4f)
    }
  }

  // -------------------------------------------------------------- building

  /** The plans a drag with a block in hand stands for. */
  def plansFor(play: Play, block: Block, start: (Int, Int), end: (Int, Int)): Vector[Plan] = {
    val (sx, sy) = start
    val (ex, ey) = end
    block.role match {
      case Role.Belt(_) =>
        val tiles =
          if(play.seeking && start != end) Builder.path(play.world, block, sx, sy, ex, ey)
          else Builder.line(sx, sy, ex, ey, play.rotation, play.horizontalFirst)
        if(tiles.size == 1) tiles.map { case (x, y, d) => Plan(block, x, y, d) } else Builder.belts(play.world, block, tiles)
      case Role.Pipe =>
        Builder.line(sx, sy, ex, ey, 0, play.horizontalFirst).map { case (x, y, _) => Plan(block, x, y, 0) }
      case Role.Zipline(reach) => chain(play, block, reach, start, end)
      case Role.Culvert(reach) => chain(play, block, reach, start, end)
      case _ =>
        val s = block.size
        val sideways = scala.math.abs(ex - sx) >= scala.math.abs(ey - sy)
        val length = if(sideways) scala.math.abs(ex - sx) else scala.math.abs(ey - sy)
        val step = Integer.signum(if(sideways) ex - sx else ey - sy)
        (0 to length / s).toVector.map { i =>
          val (x, y) = play.originFor(block, if(sideways) sx + i * s * step else sx, if(sideways) sy else sy + i * s * step)
          Plan(block, x, y, play.rotation)
        }
    }
  }

  // Ones that lead to each other, as far apart as they reach.
  private def chain(play: Play, block: Block, reach: Int, start: (Int, Int), end: (Int, Int)): Vector[Plan] = {
    val (sx, sy) = start
    val (ex, ey) = end
    val sideways = scala.math.abs(ex - sx) >= scala.math.abs(ey - sy)
    val length = if(sideways) scala.math.abs(ex - sx) else scala.math.abs(ey - sy)
    val step = Integer.signum(if(sideways) ex - sx else ey - sy)
    val stops = ((0 to length by reach).toVector :+ length).distinct
    val tiles = stops.map(d => if(sideways) (sx + d * step, sy) else (sx, sy + d * step))
    tiles.zipWithIndex.map { case ((x, y), i) =>
      Plan(block, x, y, 0, if(i + 1 < tiles.size) play.world.tileIndex(tiles(i + 1)._1, tiles(i + 1)._2) else -1)
    }
  }

  private def placeDragged(play: Play, block: Block, start: (Int, Int), end: (Int, Int)): Unit = {
    val plans = plansFor(play, block, start, end)
    var placed = 0
    var problem: String = null
    plans.foreach { p =>
      val why = play.world.placementProblem(p.block, p.x, p.y, p.rotation)
      if(why == null) { play.world.builder.add(p); placed += 1 }
      else {
        problem = why
        // Redirecting a zipline that is already there.
        play.world.buildingAt(p.x, p.y) match {
          case z: ZiplineBuilding if p.config >= 0 && (p.block eq Blocks.Zipline) => z.configure(p.config); placed += 1
          case c: CulvertBuilding if p.config >= 0 && (p.block eq Blocks.Culvert) => c.configure(p.config); placed += 1
          case _ => ()
        }
      }
    }
    if((block eq Blocks.Zipline) || (block eq Blocks.Culvert)) joinZipline(play, plans)
    if(placed > 0) play.sound(SoundKind.Place, 0.5f)
    else if(problem != null) {
      play.say(problem, Tint.Bad)
      play.sound(SoundKind.Deny, 0.5f)
    }
  }

  // Tapping one zipline after another joins them.
  private def joinZipline(play: Play, plans: Vector[Plan]): Unit = {
    val world = play.world
    if(plans.size == 1 && play.lastZipline != null) {
      val (lx, ly) = play.lastZipline
      val p = plans.head
      if(ZiplineBuilding.inReach(lx, ly, p.x, p.y, 4)) {
        val target = world.tileIndex(p.x, p.y)
        world.buildingAt(lx, ly) match {
          case z: ZiplineBuilding => z.configure(target)
          case c: CulvertBuilding => c.configure(target)
          case s: SiteBuilding => s.configure(target)
          case _ => ()
        }
        val i = world.builder.plans.indexWhere(q => q.x == lx && q.y == ly && !q.remove)
        if(i >= 0) world.builder.plans(i) = world.builder.plans(i).copy(config = target)
      }
    }
    play.lastZipline = plans.lastOption.map(p => (p.x, p.y)).orNull
  }

  def box(a: (Int, Int), b: (Int, Int)): (Int, Int, Int, Int) =
    (scala.math.min(a._1, b._1), scala.math.min(a._2, b._2), scala.math.abs(a._1 - b._1) + 1, scala.math.abs(a._2 - b._2) + 1)

  def inBox(play: Play, area: (Int, Int, Int, Int)): Vector[Building] = {
    val (x, y, w, h) = area
    play.world.buildings.iterator.filter { b =>
      b.alive && b.team == Valley.Player && !b.isInstanceOf[HearthBuilding] && b.x < x + w && b.x + b.size > x && b.y < y + h && b.y + b.size > y
    }.toVector
  }

  private def removeBox(play: Play, start: (Int, Int), end: (Int, Int)): Unit = {
    val area = box(start, end)
    play.world.builder.cancel(area._1, area._2, area._3, area._4)
    play.world.ruins.filterInPlace(r => !(r.x < area._1 + area._3 && r.x + r.block.size > area._1 && r.y < area._2 + area._4 && r.y + r.block.size > area._2))
    val targets = inBox(play, area)
    targets.foreach {
      case s: SiteBuilding => play.world.builder.add(Plan(s.target, s.x, s.y, s.rotation, remove = true))
      case b => play.world.builder.add(Plan(b.block, b.x, b.y, b.rotation, remove = true))
    }
    if(targets.nonEmpty) play.sound(SoundKind.Place, 0.5f)
  }

  private def rebuildBox(play: Play, start: (Int, Int), end: (Int, Int)): Unit = {
    val (x, y, w, h) = box(start, end)
    val chosen = play.world.ruins.filter(r => r.x < x + w && r.x + r.block.size > x && r.y < y + h && r.y + r.block.size > y).toVector
    chosen.foreach { r =>
      play.world.ruins -= r
      play.world.builder.add(Plan(r.block, r.x, r.y, r.rotation, r.config))
    }
    play.sound(if(chosen.nonEmpty) SoundKind.Place else SoundKind.Deny, 0.5f)
  }

  private def copyBox(play: Play, start: (Int, Int), end: (Int, Int)): Unit = {
    val found = inBox(play, box(start, end)).filterNot(_.isInstanceOf[SiteBuilding])
    if(found.isEmpty) play.tool = Tool.Hand
    else {
      val cx = (found.map(_.x).min + found.map(b => b.x + b.size).max) / 2
      val cy = (found.map(_.y).min + found.map(b => b.y + b.size).max) / 2
      val w = play.world.width
      play.tool = Tool.Stamp(found.map { b =>
        // Ziplines keep where they lead as a distance.
        val config = b match {
          case z: ZiplineBuilding => if(z.destination == null) -1 else encodeOffset(z.destination.x - z.x, z.destination.y - z.y)
          case c: CulvertBuilding => if(c.destination == null) -1 else encodeOffset(c.destination.x - c.x, c.destination.y - c.y)
          case _: CatapultBuilding => -1
          case _ => b.config
        }
        Plan(b.block, b.x - cx, b.y - cy, b.rotation, config, note = b.note)
      })
      val _ = w
      play.sound(SoundKind.Place, 0.4f)
    }
  }
  private def encodeOffset(dx: Int, dy: Int): Int = (dx + 8) * 32 + dy + 8
  def stamped(play: Play, plans: Vector[Plan], at: (Int, Int)): Vector[Plan] = plans.map { p =>
    val x = p.x + at._1
    val y = p.y + at._2
    val config =
      if(((p.block eq Blocks.Zipline) || (p.block eq Blocks.Culvert)) && p.config >= 0) {
        val tx = x + p.config / 32 - 8
        val ty = y + p.config % 32 - 8
        if(play.world.inside(tx, ty)) play.world.tileIndex(tx, ty) else -1
      } else p.config
    p.copy(x = x, y = y, config = config)
  }
  private def stamp(play: Play, plans: Vector[Plan], at: (Int, Int)): Unit = {
    var placed = 0
    stamped(play, plans, at).foreach { p =>
      if(play.world.knows(p.block) && play.world.placementProblem(p.block, p.x, p.y, p.rotation) == null) { play.world.builder.add(p); placed += 1 }
    }
    play.sound(if(placed > 0) SoundKind.Place else SoundKind.Deny, 0.5f)
  }
  // Mirrored from left to right.
  private def flipStamp(plans: Vector[Plan]): Vector[Plan] = plans.map { p =>
    val config = if(((p.block eq Blocks.Zipline) || (p.block eq Blocks.Culvert)) && p.config >= 0) encodeOffset(-(p.config / 32 - 8), p.config % 32 - 8) else p.config
    p.copy(x = -p.x - p.block.size, rotation = if(p.block.rotates && (p.rotation & 1) == 0) Dir.opposite(p.rotation) else p.rotation, config = config)
  }
  // A quarter turn of a layout.
  private def turnStamp(plans: Vector[Plan]): Vector[Plan] = plans.map { p =>
    val config = if(((p.block eq Blocks.Zipline) || (p.block eq Blocks.Culvert)) && p.config >= 0) {
      val dx = p.config / 32 - 8
      val dy = p.config % 32 - 8
      encodeOffset(-dy, dx)
    } else p.config
    p.copy(x = -p.y - p.block.size, y = p.x, rotation = (p.rotation + 1) & 3, config = config)
  }

  // -------------------------------------------------------------- automata

  def crew(play: Play): Vector[Creature] =
    play.world.creatures.iterator.filter(c => c.alive && c.team == Valley.Player && !UnitKinds.isLantern(c.kind)).toVector

  private def commandRelease(play: Play, start: (Int, Int), end: (Int, Int)): Unit = {
    if(play.dragPixels < 10f) {
      val p = play.pointer
      val under = crew(play).find(c => dist2(c.x, c.y, p.x, p.y) < (c.kind.radius + 8f) * (c.kind.radius + 8f))
      if(under.isEmpty && play.controls.touch && crew(play).exists(_.selected)) order(play)
      else {
        crew(play).foreach(_.selected = false)
        under.foreach(_.selected = true)
      }
    } else {
      val (x, y, w, h) = box(start, end)
      crew(play).foreach(c => c.selected = c.x >= x * T && c.x < (x + w) * T && c.y >= y * T && c.y < (y + h) * T)
    }
  }
  def order(play: Play, queue: Boolean = false): Unit = {
    val chosen = crew(play).filter(_.selected)
    if(chosen.nonEmpty) {
      val p = play.pointer
      // Side by side around the point, so that they do not push each other off it.
      val columns = scala.math.ceil(scala.math.sqrt(chosen.size.toDouble)).toInt
      chosen.zipWithIndex.foreach { case (c, i) =>
        val gap = 34f
        val tx = clamp(p.x + (i % columns - (columns - 1) / 2f) * gap, 16f, play.world.width * T - 16f)
        val ty = clamp(p.y + (i / columns - (columns - 1) / 2f) * gap, 16f, play.world.height * T - 16f)
        // Boats go as near as the water does.
        val (gx, gy) = if(c.kind.floats) play.world.standing(c.kind, tx, ty) else (tx, ty)
        if(queue) c.route.append((gx, gy))
        else {
          c.route.clear()
          c.patrolX = c.x
          c.patrolY = c.y
          c.rallyX = gx
          c.rallyY = gy
        }
        c.attackMove = true
      }
      play.world.fx(FxKind.Ring, p.x, p.y, 16f, 0x9ce07a)
      play.sound(SoundKind.Click)
    }
  }

  // ------------------------------------------------------------------ keys

  private def escape(play: Play): Unit =
    if(play.overlay == Overlay.End) ()
    else if(play.overlay == Overlay.Keys) play.overlay = Overlay.Settings
    else if(Set(Overlay.Settings, Overlay.Saves, Overlay.Deeds).contains(play.overlay)) play.overlay = Overlay.Menu
    else if(play.overlay != Overlay.None) play.overlay = Overlay.None
    else if(play.dragStart != null) play.dragStart = null
    else if(play.tool != Tool.Hand) { play.tool = Tool.Hand; play.lastZipline = null }
    else if(play.selected != null) play.selected = null
    else play.overlay = Overlay.Menu

  def toggle(play: Play, tool: Tool): Unit = {
    play.selected = null
    play.tool = if(play.tool == tool) Tool.Hand else tool
  }

  private def key(play: Play, key: Keys.Key): Unit = {
    val command = play.profile.bindings.commandOf(key)
    def overlay(o: Overlay): Unit = if(play.overlay == o) play.overlay = Overlay.None else if(play.overlay == Overlay.None) play.overlay = o
    if(play.binding.nonEmpty) {
      if(key != Keys.Escape && Bindings.all.contains(key)) play.profile.bindings.bind(play.binding.get, key)
      play.binding = None
    } else if(key == Keys.Escape) escape(play)
    else if(key == Keys.F11) play.services.fullscreen = !play.services.fullscreen
    else command match {
      case Some(Command.Learn) => overlay(Overlay.Learn)
      case Some(Command.Help) => overlay(Overlay.Help)
      case Some(Command.Codex) =>
        pointed(play).foreach(b => if(play.overlay == Overlay.None) play.codexBlock = b)
        overlay(Overlay.Codex)
      case Some(Command.Map) => overlay(Overlay.Map)
      case Some(Command.Ledger) => overlay(Overlay.Ledger)
      case Some(Command.Layouts) => overlay(Overlay.Layouts)
      case _ if play.overlay != Overlay.None => ()
      case Some(Command.Pause) => play.paused = !play.paused
      case Some(Command.Speed) => play.speed = if(play.speed == 1) 2 else if(play.speed == 2) 4 else 1
      case Some(Command.Turn) => turn(play)
      case Some(Command.Flip) => flip(play)
      case Some(Command.Remove) => toggle(play, Tool.Remove)
      case Some(Command.Copy) => toggle(play, Tool.Copy)
      case Some(Command.Rebuild) => toggle(play, Tool.Rebuild)
      case Some(Command.Crew) => toggle(play, Tool.Command)
      case Some(Command.AllCrew) =>
        play.tool = Tool.Command
        crew(play).foreach(_.selected = true)
      case Some(Command.Ride) => ride(play)
      case Some(Command.Pick) => pick(play)
      case Some(Command.KeepRuins) => play.world.builder.rebuild = !play.world.builder.rebuild; play.say(if(play.world.builder.rebuild) "Ruins are rebuilt" else "Ruins are left alone")
      case Some(Command.StopBuilding) => play.world.builder.paused = !play.world.builder.paused; play.say(if(play.world.builder.paused) "Building stopped" else "Building goes on")
      case Some(Command.DropPlans) => play.world.builder.clear(); play.say("Plans dropped")
      case Some(Command.NextWave) => if(!play.world.rules.peaceful) play.world.waves.launchNow()
      case Some(Command.Hide) => play.hidden = !play.hidden
      case _ => key match {
        case n: Keys.NumKey => number(play, n.toInt)
        case _ => ()
      }
    }
  }

  // With automata commanded, numbers are groups; otherwise they choose what to build.
  private def number(play: Play, n: Int): Unit =
    if(play.tool == Tool.Command) {
      if(play.controls.shift) {
        play.groups(n) = crew(play).filter(_.selected).map(_.id).toSet
        play.say(s"Group $n")
      } else crew(play).foreach(c => c.selected = play.groups(n).contains(c.id))
    } else if(n >= 1 && n <= Category.values.length) play.category = Category.fromOrdinal(n - 1)

  def flip(play: Play): Unit = play.tool match {
    case Tool.Stamp(plans) => play.tool = Tool.Stamp(flipStamp(plans))
    case _ => ()
  }
  def turn(play: Play): Unit = play.tool match {
    case Tool.Stamp(plans) => play.tool = Tool.Stamp(turnStamp(plans))
    case Tool.Hand =>
      // What is pointed at, where it stands.
      val (tx, ty) = play.pointerTile
      play.world.buildingAt(tx, ty) match {
        case null => play.rotation = (play.rotation + 1) & 3
        case b if b.team == Valley.Player && b.block.rotates && !b.isInstanceOf[SiteBuilding] =>
          b.rotation = (b.rotation + (if(play.controls.shift) 3 else 1)) & 3
          b.neighboursChanged()
          play.world.refresh(b)
          play.sound(SoundKind.Click, 0.4f)
        case _ => ()
      }
    case _ => play.rotation = (play.rotation + (if(play.controls.shift) 3 else 1)) & 3
  }

  def ride(play: Play): Unit =
    if(play.riding != null) {
      play.riding.controlled = false
      play.riding.rallyX = play.riding.x
      play.riding.rallyY = play.riding.y
      play.riding = null
    } else {
      val p = play.pointer
      crew(play).filter(c => dist2(c.x, c.y, p.x, p.y) < (c.kind.radius + 24f) * (c.kind.radius + 24f)).sortBy(c => dist2(c.x, c.y, p.x, p.y)).headOption.foreach { c =>
        play.riding = c
        c.controlled = true
        c.route.clear()
        play.following = true
        play.tool = Tool.Hand
        play.world.mining = -1
        play.sound(SoundKind.Place)
      }
    }

  /** The block that is pointed at. */
  private def pointed(play: Play): Option[Block] = {
    val (tx, ty) = play.pointerTile
    play.world.buildingAt(tx, ty) match {
      case null => play.tool match { case Tool.Build(b) => Some(b); case _ => None }
      case s: SiteBuilding => Some(s.target)
      case b => Some(b.block)
    }
  }

  private def pick(play: Play): Unit = {
    val (tx, ty) = play.pointerTile
    val b = play.world.buildingAt(tx, ty)
    val block = b match {
      case null => null
      case s: SiteBuilding => s.target
      case other => other.block
    }
    if(block != null && block.category.nonEmpty && play.world.knows(block)) {
      play.tool = Tool.Build(block)
      play.rotation = b.rotation
      play.category = block.category.get
      play.sound(SoundKind.Click, 0.4f)
    } else { play.tool = Tool.Hand; play.lastZipline = null }
  }
}
