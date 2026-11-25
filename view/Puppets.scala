package hearthwheel
package core

import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d.{Painter2D, Render2D, Texture2D}
import sgl.engine.gameobject.spatial2d._
import hearthwheel.model._
import hearthwheel.model.MathUtil._
import sgl.geometry.Vec2

/** A creature of the world as a game object: it follows the creature, and is
  * drawn by [[PuppetRenderSystem]]. It goes when the creature does.
  */
final class Puppet(obj: GameObject2D, val creature: Creature) extends ComponentOf(obj) {
  val spatial: Spatial2D = obj.spatial
  /** Eased, for wings and for the lean of what flies. */
  var lift = 0f
}

/** Keeps one puppet for every creature, and moves them between two steps. */
object PuppetSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.puppets")

  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    val between = if(play.running) context.global[FixedStepProgress].value else 1f
    val known = play.puppets
    play.world.creatures.foreach { c =>
      if(c.alive && !known.contains(c.id)) known(c.id) = spawn(context, c)
    }
    context.world.index[Puppet].foreach { puppet =>
      val c = puppet.creature
      if(!c.alive) {
        val _ = known.remove(c.id)
        puppet.destroy()
      } else {
        puppet.spatial.local = Transform2D(Vec2(lerp(c.beforeX, c.x, between), lerp(c.beforeY, c.y, between)), c.rotation)
      }
    }
  }
  private def spawn(context: GameContext, c: Creature)(using Scope): Puppet =
    context.spawn2D(c.kind.key, Transform2D(Vec2(c.x, c.y), c.rotation)).attach(new Puppet(_, c)).get[Puppet]
}

object PuppetRenderSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.puppet-render")

  val GroundZ = 5f
  val AirZ = 7f

  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    val draw = context.global[Render2D]
    val time = play.time
    draw.world(GroundZ) { p => eachSeen(context, play, flies = false)(paint(p, play.art, _, time)) }
    draw.world(AirZ) { p =>
      eachSeen(context, play, flies = true) { puppet =>
        val at = puppet.spatial.global.position
        val r = puppet.creature.kind.radius
        p.sprite(play.art.shadow, at.x + 9f, at.y + 22f, r * 2.2f, r * 1.7f, 0f, Color(1f, 1f, 1f, 0.28f))
      }
      eachSeen(context, play, flies = true)(paint(p, play.art, _, time))
    }
    draw.world(8.5f) { p => eachSeen2(context, play)(bar(p, _)) }
  }

  private def eachSeen(context: GameContext, play: Play, flies: Boolean)(f: Puppet => Unit): Unit =
    context.world.index[Puppet].foreach { puppet => if(puppet.creature.kind.flies == flies && play.sees(puppet.creature.x, puppet.creature.y, 60f)) f(puppet) }
  private def eachSeen2(context: GameContext, play: Play)(f: Puppet => Unit): Unit =
    context.world.index[Puppet].foreach { puppet => if(play.sees(puppet.creature.x, puppet.creature.y, 60f)) f(puppet) }

  private def legs(art: Gallery, k: UnitKind): Texture2D =
    if(k.friendly) art.legBrass
    else if((k eq UnitKinds.Rimewood) || (k eq UnitKinds.Frostling)) art.legIce
    else if((k eq UnitKinds.Stumper) || (k eq UnitKinds.Gnarl) || (k eq UnitKinds.Catkin)) art.legWood
    else art.legVine

  private def paint(p: Painter2D, art: Gallery, puppet: Puppet, time: Float): Unit = {
    val c = puppet.creature
    val k = c.kind
    val at = puppet.spatial.global.position
    val rot = puppet.spatial.global.rotation
    val x = at.x
    val y = at.y
    val r = k.radius
    val cosR = cos(rot)
    val sinR = sin(rot)
    val still = c.frozen > 0f
    val t = if(still) 0f else time

    if(k.floats) {
      val speed = scala.math.min(1f, (scala.math.abs(c.vx) + scala.math.abs(c.vy)) / k.speed)
      if(speed > 0.1f) p.sprite(art.wake, x - cosR * r * 1.1f, y - sinR * r * 1.1f, r * 3.4f, r * 2.6f, rot, Color(1f, 1f, 1f, 0.75f * speed))
    }
    if(!k.flies) {
      p.sprite(art.shadow, x + 2f, y + 4f, r * 2.5f, r * 2.2f, 0f, Color(1f, 1f, 1f, 0.3f))
      // Legs swing with the ground covered, each half of them in turn.
      if(k.legs > 0) {
        val leg = legs(art, k)
        val perSide = k.legs / 2
        var i = 0
        while(i < k.legs) {
          val side = if(i < perSide) -1f else 1f
          val n = i % perSide
          val spread = if(perSide == 1) 0f else (n.toFloat / (perSide - 1) - 0.5f) * 2f
          val phase = sin(c.walked * 0.22f * (12f / scala.math.max(8f, r)) + n * 2.1f + (if(side > 0f) Pi else 0f))
          val a = side * (Pi / 2f - spread * 0.75f) + phase * 0.35f * side
          val la = rot + a
          val len = r * 1.25f
          p.sprite(leg, x + cos(la) * (r * 0.5f + len * 0.45f), y + sin(la) * (r * 0.5f + len * 0.45f), len, len * 0.26f, la)
          i += 1
        }
      }
    }
    k.key match {
      case "lantern_great" | "lantern_grand" => flaps(p, art.wingPaper, x, y, rot, r * 0.95f, r * 1.8f, time * 14f, -0.2f, 1.05f)
      case "heron" => flaps(p, art.wingSail, x, y, rot, r * 0.2f, r * 2.7f, time * 5f, -0.05f, 1f)
      case "wren" => flaps(p, art.wingPaper, x, y, rot, r * 0.3f, r * 1.7f, time * 26f, -0.1f, 1f)
      case "mason" => flaps(p, art.wingGlass, x, y, rot, r * 0.3f, r * 1.9f, time * 30f, -0.1f, 1f)
      case "tender" => flaps(p, art.wingGlass, x, y, rot, r * 0.4f, r * 2f, time * 24f, -0.1f, 1f)
      case "dewmoth" => flaps(p, art.wingDew, x, y, rot, r * 0.2f, r * 2.2f, t * 9f, -0.05f, 0.9f)
      case "queen" =>
        flaps(p, art.wingLeaf, x, y, rot, r * 0.4f, r * 2.2f, t * 26f, 0.05f, 0.85f)
        flaps(p, art.wingLeaf, x - cosR * r * 0.4f, y - sinR * r * 0.4f, rot, r * 0.3f, r * 1.8f, t * 26f + 1f, -0.15f, 1.2f)
      case "lantern" => flaps(p, art.wingPaper, x, y, rot, r * 0.9f, r * 1.5f, time * 14f, -0.2f, 1.05f)
      case "lanternfly" => flaps(p, art.wingGlass, x, y, rot, r * 0.3f, r * 2.3f, time * 32f, -0.05f, 0.8f); flaps(p, art.wingGlass, x - cosR * r * 0.5f, y - sinR * r * 0.5f, rot, r * 0.2f, r * 1.8f, time * 32f + 1f, -0.2f, 1.15f)
      case "hornwing" => flaps(p, art.wingLeaf, x, y, rot, r * 0.3f, r * 2.1f, t * 30f, -0.1f, 0.95f)
      case _ => ()
    }
    val wobble = if(k.key == "burr") c.walked * 0.09f else if(k.key == "dandel" || k.key == "stormseed") t * 0.8f else 0f
    val bob = if(k.flies) sin(time * 2.4f + c.id) * 1.6f else 0f
    val s = k.radius * (if(k.boss) 2.7f else if(k.key == "burr") 3f else 2.75f)
    val kick = c.recoil * 2.5f
    val color =
      if(still) Color(0.65f, 0.88f, 1f, 1f)
      else if(c.burn > 0f) Color(1f, 0.8f, 0.65f, 1f)
      else if(c.slow > 0f) Color(0.85f, 0.95f, 1f, 1f)
      else Color.White
    val body = art.bodies(k.id)
    p.sprite(body, x - cosR * kick, y - sinR * kick + bob, s, s, rot + wobble, color)
    if(c.hitAgo < 0.1f) p.additive(p.sprite(body, x, y + bob, s, s, rot + wobble, Color(1f, 1f, 1f, 0.75f)))
    if(UnitKinds.isLantern(k)) p.additive(p.sprite(art.glow, x, y + bob, 110f + r * 3f, 110f + r * 3f, 0f, Color(1f, 0.75f, 0.4f, 0.24f + 0.03f * sin(time * 5f))))
    if(c.maxShield > 0f && c.shield > 0f) p.additive(p.sprite(art.ring, x, y + bob, r * 3f, r * 3f, 0f, Color(0.5f, 0.85f, 1f, 0.25f + 0.5f * c.shield / c.maxShield)))
    if(c.guardian) p.sprite(art.crown, x, y - r - 12f + sin(time * 3f) * 1.5f, 15f, 15f)
    if(c.cargoAmount > 0 && c.cargo != null) p.sprite(art.items(c.cargo.id), x - cosR * r * 0.8f, y - sinR * r * 0.8f, 10f, 10f)
    if(c.controlled) p.sprite(art.ring, x, y + 2f, r * 3.1f, r * 3.1f, 0f, Color(1f, 0.85f, 0.4f, 0.9f))
    if(still) p.sprite(art.frost, x, y, r * 2.4f, r * 2.4f, 0f, Tint.alpha(0.9f))
    if(c.selected) p.sprite(art.ring, x, y + 2f, r * 2.9f, r * 2.9f, 0f, Color(0.6f, 1f, 0.7f, 0.9f))
  }

  private def flaps(p: Painter2D, wing: Texture2D, x: Float, y: Float, rot: Float, root: Float, size: Float, phase: Float, back: Float, spread: Float): Unit = {
    val beat = 0.75f + 0.25f * sin(phase)
    var side = -1
    while(side <= 1) {
      val a = rot + side * (Pi / 2f) * spread
      val wx = x + cos(a) * (root + size * 0.42f) + cos(rot) * size * back
      val wy = y + sin(a) * (root + size * 0.42f) + sin(rot) * size * back
      // Drawn with the root at the bottom left; mirrored for the other side.
      if(side > 0) p.sprite(wing, wx, wy, size, size * 0.7f * beat, a, Color.White, 0f, 1f, 1f, 0f)
      else p.sprite(wing, wx, wy, size, size * 0.7f * beat, a)
      side += 2
    }
  }

  private def bar(p: Painter2D, puppet: Puppet): Unit = {
    val c = puppet.creature
    if(c.hp < c.maxHp && !c.kind.boss) {
      val at = puppet.spatial.global.position
      val w = c.kind.radius * 2f
      val y = at.y - c.kind.radius - 8f
      p.rect(at.x - w / 2f - 1f, y - 1f, w + 2f, 4.4f, Color(0.1f, 0.06f, 0.1f, 0.7f))
      p.rect(at.x - w / 2f, y, w * scala.math.max(0f, c.hp / c.maxHp), 2.4f, if(c.team == Valley.Player) Tint.Good else Tint.Bad)
    }
  }
}
