package hearthwheel
package core

import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d.{Painter2D, Render2D}
import hearthwheel.model._
import hearthwheel.model.MathUtil._

/** What is planned, what is in hand, and what is pointed at. */
object PlanRenderSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.plans")
  private val T = Valley.Tile.toFloat

  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    val draw = context.global[Render2D]
    draw.world(4.5f)(planned(play, _))
    draw.world(10f)(inHand(play, _))
  }

  private def planned(play: Play, p: Painter2D): Unit = {
    val world = play.world
    world.ruins.foreach { r =>
      val s = r.block.size * T
      val x = r.x * T + s / 2f
      val y = r.y * T + s / 2f
      if(play.sees(x, y, s)) Portrait.draw(p, play.art, r.block, x, y, s, r.rotation, Color(1f, 0.6f, 0.5f, 0.16f))
    }
    world.builder.plans.foreach { plan =>
      if(play.sees(plan.cx, plan.cy, 100f)) {
        val s = plan.block.size * T
        if(plan.remove) {
          p.rect(plan.x * T, plan.y * T, s, s, Color(1f, 0.3f, 0.2f, 0.28f))
          p.sprite(play.art.cross, plan.cx, plan.cy, s * 0.5f, s * 0.5f, 0f, Tint.alpha(0.8f))
        } else world.buildingAt(plan.x, plan.y) match {
          case s0: SiteBuilding if s0.target eq plan.block => ()
          case _ => Portrait.draw(p, play.art, plan.block, plan.cx, plan.cy, s, plan.rotation, Color(0.7f, 0.9f, 1f, 0.45f))
        }
      }
    }
    val lantern = world.lantern
    val site = world.builder.active
    if(lantern != null && site != null) {
      // The light it works by.
      p.additive {
        p.line(lantern.x, lantern.y, site.cx, site.cy, 3f + sin(play.time * 20f), Color(1f, 0.8f, 0.4f, 0.4f))
        val s = site.size * T
        p.sprite(play.art.glow, site.cx, site.cy, s * 1.6f, s * 1.6f, 0f, Color(1f, 0.75f, 0.4f, 0.3f))
      }
    }
    if(world.frozen) warmth(play, p)
  }

  // The edge of what is warm, while placing something that minds the cold.
  private def warmth(play: Play, p: Painter2D): Unit = {
    val placing = play.tool match {
      case Tool.Build(b) => b.chills || b.role.isInstanceOf[Role.Brazier]
      case _ => false
    }
    if(placing) play.view.shown.foreach {
      case h: HearthBuilding => ring(play, p, h.cx, h.cy, h.block.warmthRadius * T, Color(1f, 0.6f, 0.3f, 0.7f))
      case b: BrazierBuilding => if(b.lit) ring(play, p, b.cx, b.cy, b.radius * T, Color(1f, 0.6f, 0.3f, 0.7f))
      case r: RadiatorBuilding => if(r.lit) ring(play, p, r.cx, r.cy, r.radius * T, Color(1f, 0.6f, 0.3f, 0.7f))
      case _ => ()
    }
  }

  private def ring(play: Play, p: Painter2D, x: Float, y: Float, r: Float, color: Color): Unit = {
    val n = scala.math.max(16, (r / 9f).toInt)
    var i = 0
    while(i < n) {
      val a = (i + (play.time * 0.6f) % 1f) * Tau / n
      p.sprite(play.art.dash, x + cos(a) * r, y + sin(a) * r, 10f, 3.4f, a + Pi / 2f, color)
      i += 1
    }
  }

  private def corners(play: Play, p: Painter2D, x: Float, y: Float, w: Float, h: Float, color: Color): Unit = {
    val b = 2f + sin(play.time * 5f)
    val s = 12f
    p.sprite(play.art.corner, x - b + s / 2f, y - b + s / 2f, s, s, 0f, color)
    p.sprite(play.art.corner, x + w + b - s / 2f, y - b + s / 2f, s, s, Pi / 2f, color)
    p.sprite(play.art.corner, x + w + b - s / 2f, y + h + b - s / 2f, s, s, Pi, color)
    p.sprite(play.art.corner, x - b + s / 2f, y + h + b - s / 2f, s, s, -Pi / 2f, color)
  }

  private def ghost(play: Play, p: Painter2D, plan: Plan): Boolean = {
    val ok = play.world.placementProblem(plan.block, plan.x, plan.y, plan.rotation) == null
    val s = plan.block.size * T
    Portrait.draw(p, play.art, plan.block, plan.cx, plan.cy, s, plan.rotation, if(ok) Color(0.85f, 1f, 0.85f, 0.75f) else Color(1f, 0.45f, 0.4f, 0.6f))
    p.rect(plan.x * T, plan.y * T, s, s, if(ok) Color(0.6f, 1f, 0.6f, 0.12f) else Color(1f, 0.2f, 0.15f, 0.25f))
    ok
  }

  private def rangeOf(play: Play, p: Painter2D, block: Block, x: Float, y: Float): Unit = block.role match {
    case _: Role.Turret | _: Role.Mender | Role.Aegis(_, _) | Role.Metronome(_, _) | Role.Catapult(_) => ring(play, p, x, y, block.range * T, Color(1f, 0.95f, 0.75f, 0.85f))
    case _: Role.Brazier | _: Role.Radiator => ring(play, p, x, y, block.range * T, Color(1f, 0.6f, 0.3f, 0.85f))
    case Role.Pylon(reach, _) => ring(play, p, x, y, (reach - 1f + block.size / 2f) * T, Color(1f, 0.85f, 0.45f, 0.75f))
    case _ => ()
  }

  private def inHand(play: Play, p: Painter2D): Unit = if(play.overlay == Overlay.None && !play.hidden) {
    val world = play.world
    if(world.mining >= 0 && world.lantern != null) {
      val mx = (world.mining % world.width + 0.5f) * T
      val my = (world.mining / world.width + 0.5f) * T
      p.additive(p.line(world.lantern.x, world.lantern.y, mx, my, 2.5f + sin(play.time * 18f), Color(1f, 0.85f, 0.5f, 0.6f)))
      corners(play, p, mx - 16f, my - 16f, T, T, Tint.Gold)
    }
    val selected = play.selected
    if(selected != null && selected.alive) {
      corners(play, p, selected.x * T, selected.y * T, selected.size * T, selected.size * T, Tint.Gold)
      rangeOf(play, p, selected.block, selected.cx, selected.cy)
      selected match {
        case z: ZiplineBuilding =>
          val r = z.reach * T
          p.rect(z.cx - r, z.y * T, r * 2f, T, Color(1f, 0.85f, 0.5f, 0.12f))
          p.rect(z.x * T, z.cy - r, T, r * 2f, Color(1f, 0.85f, 0.5f, 0.12f))
        case _ => ()
      }
    }
    if((play.overUi || play.controls.touch) && play.dragStart == null) return
    val (tx, ty) = play.pointerTile
    val start = play.dragStart
    val removing = start != null && play.dragButton == 1 && play.tool != Tool.Command
    if(removing) { if(start != (tx, ty)) removal(play, p, start, (tx, ty)) }
    else play.tool match {
      case Tool.Build(block) =>
        val plans = InputSystem.plansFor(play, block, if(start != null) start else (tx, ty), (tx, ty))
        plans.foreach { plan => val _ = ghost(play, p, plan) }
        plans.lastOption.foreach { plan =>
          Facts.atPlace(play.world, plan).foreach { text =>
            p.text(text, sgl.geometry.Vec2(plan.cx - sgl.engine.gameobject.render2d.Text2D.width(text, 11f, bold = true) / 2f, plan.y * T - 18f), 11f, Tint.Cream, bold = true)
          }
          if(block.rotates) p.sprite(play.art.arrow, plan.cx + Dir.dx(plan.rotation) * 30f, plan.cy + Dir.dy(plan.rotation) * 30f, 16f, 16f, plan.rotation * Pi / 2f, Tint.alpha(0.8f))
          rangeOf(play, p, block, plan.cx, plan.cy)
          block.role match {
            case Role.Pylon(_, _) => pylonPreview(play, p, plan)
            case Role.Culvert(reach) if play.lastZipline != null =>
              val (lx, ly) = play.lastZipline
              if(ZiplineBuilding.inReach(lx, ly, plan.x, plan.y, reach)) p.line((lx + 0.5f) * T, (ly + 0.5f) * T, plan.cx, plan.cy, 2.5f, Tint.Water)
            case Role.Zipline(reach) if play.lastZipline != null =>
              val (lx, ly) = play.lastZipline
              if(ZiplineBuilding.inReach(lx, ly, plan.x, plan.y, reach)) p.line((lx + 0.5f) * T, (ly + 0.5f) * T, plan.cx, plan.cy, 2.5f, Color(1f, 0.9f, 0.6f, 0.7f))
            case _ => ()
          }
        }
      case Tool.Remove =>
        if(start != null) removal(play, p, start, (tx, ty))
        else {
          val b = world.buildingAt(tx, ty)
          if(b != null && b.team == Valley.Player) p.rect(b.x * T, b.y * T, b.size * T, b.size * T, Color(1f, 0.3f, 0.2f, 0.35f))
          else p.rect(tx * T, ty * T, T, T, Color(1f, 0.3f, 0.2f, 0.2f))
        }
      case Tool.Copy =>
        val (x, y, w, h) = InputSystem.box(if(start != null) start else (tx, ty), (tx, ty))
        p.rect(x * T, y * T, w * T, h * T, Color(0.5f, 0.8f, 1f, 0.2f))
        corners(play, p, x * T, y * T, w * T, h * T, Color(0.6f, 0.9f, 1f))
      case Tool.Stamp(plans) => InputSystem.stamped(play, plans, (tx, ty)).foreach { plan => val _ = ghost(play, p, plan) }
      case Tool.Rebuild =>
        val (x, y, w, h) = InputSystem.box(if(start != null) start else (tx, ty), (tx, ty))
        p.rect(x * T, y * T, w * T, h * T, Color(1f, 0.85f, 0.4f, 0.16f))
        corners(play, p, x * T, y * T, w * T, h * T, Tint.Gold)
      case Tool.Command =>
        if(start != null && play.dragButton == 0 && play.dragPixels >= 10f) {
          val (x, y, w, h) = InputSystem.box(start, (tx, ty))
          p.rect(x * T, y * T, w * T, h * T, Color(0.5f, 1f, 0.6f, 0.15f))
          corners(play, p, x * T, y * T, w * T, h * T, Tint.Good)
        }
        world.creatures.foreach { c =>
          if(c.selected) {
            p.line(c.x, c.y, c.rallyX, c.rallyY, 1.4f, Color(0.6f, 1f, 0.7f, 0.3f))
            var (lx, ly) = (c.rallyX, c.rallyY)
            c.route.foreach { case (nx, ny) => p.line(lx, ly, nx, ny, 1.4f, Color(0.6f, 1f, 0.7f, 0.22f)); lx = nx; ly = ny }
            p.sprite(play.art.dash, c.rallyX, c.rallyY, 10f, 10f, Pi / 4f, Color(0.6f, 1f, 0.7f, 0.5f))
          }
        }
      case Tool.Hand =>
        val b = world.buildingAt(tx, ty)
        selected match {
          case loom: LoomBuilding => loom.ties.indices.foreach { i =>
            val o = loom.tied(i)
            if(o != null) {
              p.line(loom.cx, loom.cy, o.cx, o.cy, 2.4f, Color(0.95f, 0.85f, 1f, 0.7f))
              corners(play, p, o.x * T, o.y * T, o.size * T, o.size * T, Tint(0xd8b4ff))
              p.text((i + 1).toString, sgl.geometry.Vec2(o.cx - 5f, o.cy - 10f), 16f, Color.White, true)
            }
          }
          case _ => ()
        }
        if(b == null && world.inside(tx, ty) && world.canMine(world.tileIndex(tx, ty)) && world.mining != world.tileIndex(tx, ty)) corners(play, p, tx * T, ty * T, T, T, Color(1f, 0.85f, 0.4f, 0.5f))
        if(b != null && (b ne selected) && !play.controls.touch) corners(play, p, b.x * T, b.y * T, b.size * T, b.size * T, Tint.alpha(0.6f))
        if(play.shooting) {
          val at = play.pointer
          p.sprite(play.art.ring, at.x, at.y, 22f, 22f, 0f, Color(1f, 0.7f, 0.4f, 0.9f))
        }
    }
  }

  private def removal(play: Play, p: Painter2D, start: (Int, Int), end: (Int, Int)): Unit = {
    val (x, y, w, h) = InputSystem.box(start, end)
    p.rect(x * T, y * T, w * T, h * T, Color(1f, 0.3f, 0.2f, 0.16f))
    corners(play, p, x * T, y * T, w * T, h * T, Tint.Bad)
    InputSystem.inBox(play, (x, y, w, h)).foreach(b => p.rect(b.x * T, b.y * T, b.size * T, b.size * T, Color(1f, 0.3f, 0.2f, 0.35f)))
  }

  private def pylonPreview(play: Play, p: Painter2D, plan: Plan): Unit = play.world.make(plan.block, plan.x, plan.y, 0, Valley.Player) match {
    case pylon: PylonBuilding =>
      play.view.shown.foreach { b =>
        if(b.team == Valley.Player && b.isPowered && !b.isInstanceOf[SiteBuilding] && pylon.canLink(b) && (b.isInstanceOf[PylonBuilding] || b.links.isEmpty))
          p.line(plan.cx, plan.cy, b.cx, b.cy, 2f, Color(1f, 0.9f, 0.5f, 0.55f))
      }
    case _ => ()
  }
}
