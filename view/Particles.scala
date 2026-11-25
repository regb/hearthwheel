package hearthwheel
package core

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.render2d.{Painter2D, Texture2D}
import hearthwheel.model.MathUtil._
import hearthwheel.model.{Fx, FxKind, Rng}

/** A particle in flight. Reused once it has faded. */
private final class Particle {
  var texture: Texture2D = null
  var x = 0f
  var y = 0f
  var vx = 0f
  var vy = 0f
  var drag = 0f
  var rise = 0f
  var rotation = 0f
  var spin = 0f
  var age = 0f
  var life = 1f
  var size0 = 1f
  var size1 = 1f
  var aspect = 1f
  var color: Color = Color.White
  var additive = false
}

/** Thousands of short-lived things: smoke, sparks, chips, rings of light. */
final class Particles(art: Gallery) {
  private val rng = new Rng(99L)
  private val Max = 6000
  private val live = new scala.collection.mutable.ArrayBuffer[Particle]
  private val pool = new scala.collection.mutable.ArrayBuffer[Particle]
  // Lines and beams, which have two ends.
  private final class Streak(val x: Float, val y: Float, val x2: Float, val y2: Float, val color: Color, val life: Float, val straight: Boolean = false) { var age = 0f }
  private val streaks = new scala.collection.mutable.ArrayBuffer[Streak]

  def count: Int = live.size

  private def add(texture: Texture2D, x: Float, y: Float, vx: Float, vy: Float, life: Float, size0: Float, size1: Float, color: Color, additive: Boolean = false, drag: Float = 2f, spin: Float = 0f, aspect: Float = 1f, rotation: Float = 0f, rise: Float = 0f): Unit =
    if(live.size < Max) {
      val p = if(pool.nonEmpty) pool.remove(pool.size - 1) else new Particle
      p.texture = texture
      p.x = x; p.y = y; p.vx = vx; p.vy = vy
      p.drag = drag; p.rise = rise
      p.rotation = rotation; p.spin = spin
      p.age = 0f; p.life = life
      p.size0 = size0; p.size1 = size1; p.aspect = aspect
      p.color = color
      p.additive = additive
      live += p
    }

  private def burst(n: Int)(f: (Float, Float) => Unit): Unit = {
    var i = 0
    while(i < n) { f(rng.float() * Tau, rng.float()); i += 1 }
  }

  def emit(e: Fx): Unit = e.kind match {
    case FxKind.Puff =>
      burst(4)((a, k) => add(art.puff, e.x + cos(a) * 4f, e.y + sin(a) * 4f, cos(a) * 14f * k, sin(a) * 14f * k - 10f, 0.9f + k * 0.5f, 10f, 26f, Color(0.9f, 0.87f, 0.83f, 0.5f), rise = -14f))
    case FxKind.Hit =>
      val c = Tint(e.color)
      burst(5)((_, k) => { val a = e.a + rng.range(-0.7f, 0.7f); add(art.spark, e.x, e.y, cos(a) * 120f * k, sin(a) * 120f * k, 0.22f, 9f, 3f, c, additive = true, aspect = 0.35f, rotation = a) })
      add(art.glow, e.x, e.y, 0f, 0f, 0.14f, 26f, 10f, c.withAlpha(0.8f), additive = true)
    case FxKind.Muzzle =>
      val c = Tint(e.color)
      add(art.glow, e.x, e.y, 0f, 0f, 0.1f, 24f, 14f, c, additive = true)
      add(art.spark, e.x + cos(e.a) * 6f, e.y + sin(e.a) * 6f, cos(e.a) * 60f, sin(e.a) * 60f, 0.1f, 18f, 8f, Color(1f, 0.95f, 0.8f, 1f), additive = true, aspect = 0.4f, rotation = e.a)
    case FxKind.Explosion =>
      val c = Tint(e.color)
      val r = e.a
      add(art.glow, e.x, e.y, 0f, 0f, 0.35f, r * 3.2f, r * 1.6f, c, additive = true)
      add(art.ring, e.x, e.y, 0f, 0f, 0.35f, r * 0.6f, r * 2.2f, c.withAlpha(0.9f), additive = true)
      burst((6 + r * 0.25f).toInt)((a, k) => add(art.puff, e.x + cos(a) * r * 0.4f * k, e.y + sin(a) * r * 0.4f * k, cos(a) * r * 1.6f * k, sin(a) * r * 1.6f * k, 0.5f + k * 0.5f, r * 0.45f, r * 0.9f, Color(0.32f, 0.27f, 0.3f, 0.55f), rise = -10f))
      burst((8 + r * 0.2f).toInt)((a, k) => add(art.spark, e.x, e.y, cos(a) * r * 5f * k, sin(a) * r * 5f * k, 0.35f + k * 0.2f, 12f, 3f, c, additive = true, aspect = 0.3f, rotation = a, drag = 3f))
    case FxKind.Flame =>
      add(art.mote, e.x, e.y, rng.range(-8f, 8f), -22f, 0.5f, 12f, 3f, Color(1f, 0.55f, 0.2f, 0.9f), additive = true)
      add(art.mote, e.x, e.y, rng.range(-5f, 5f), -26f, 0.35f, 7f, 2f, Color(1f, 0.9f, 0.5f, 0.9f), additive = true)
    case FxKind.Mist =>
      val v = rng.range(120f, 190f)
      add(art.mote, e.x, e.y, cos(e.a) * v, sin(e.a) * v, 0.75f, 7f, 22f, Color(0.75f, 0.92f, 1f, 0.5f), drag = 1.2f)
    case FxKind.Mend =>
      val r = e.a * 16f
      burst(3)((a, k) => add(art.mote, e.x + cos(a) * r * k, e.y + sin(a) * r * k, 0f, -18f, 0.8f, 7f, 2f, Color(0.6f, 1f, 0.6f, 0.9f), additive = true))
    case FxKind.Build =>
      val r = e.a * 18f
      add(art.ring, e.x, e.y, 0f, 0f, 0.3f, r * 1.4f, r * 2.8f, Color(1f, 0.85f, 0.5f, 0.9f), additive = true)
      burst(6)((a, k) => add(art.chip, e.x + cos(a) * r, e.y + sin(a) * r, cos(a) * 40f * k, sin(a) * 40f * k, 0.5f, 5f, 2f, Tint(0xd8a878), spin = 8f * (k - 0.5f), drag = 4f))
      burst(4)((a, k) => add(art.puff, e.x + cos(a) * r, e.y + sin(a) * r, cos(a) * 24f, sin(a) * 24f, 0.5f + 0.3f * k, 10f, 20f, Color(0.95f, 0.9f, 0.8f, 0.45f)))
    case FxKind.Break =>
      val r = e.a * 14f
      burst((6 + e.a * 6f).toInt)((a, k) => add(art.chip, e.x + cos(a) * r * k, e.y + sin(a) * r * k, cos(a) * 110f * k, sin(a) * 110f * k, 0.7f, 7f, 3f, if(k > 0.5f) Tint(0xb0744a) else Tint(0x6b5a58), spin = 14f * (k - 0.5f), drag = 4f))
      burst((3 + e.a * 3f).toInt)((a, k) => add(art.puff, e.x + cos(a) * r * k, e.y + sin(a) * r * k, cos(a) * 30f * k, sin(a) * 30f * k, 0.9f + k * 0.5f, 14f, 34f, Color(0.4f, 0.36f, 0.36f, 0.6f), rise = -12f))
    case FxKind.Death =>
      val c = Tint(e.color)
      val r = e.a
      burst((5 + r * 0.5f).toInt)((a, k) => add(art.leaf, e.x, e.y, cos(a) * 90f * k, sin(a) * 90f * k, 0.6f + 0.4f * k, 8f + r * 0.2f, 4f, if(k > 0.6f) Tint(0x7a4a6a) else c, spin = 10f * (k - 0.5f), drag = 3f, aspect = 0.66f, rotation = a))
      burst(3)((a, k) => add(art.puff, e.x, e.y, cos(a) * 20f, sin(a) * 20f, 0.5f + 0.3f * k, r, r * 2f, c.withAlpha(0.4f)))
    case FxKind.Beam =>
      streaks += new Streak(e.x, e.y, e.x2, e.y2, Tint(e.color), 0.4f, straight = true)
      add(art.glow, e.x, e.y, 0f, 0f, 0.25f, 60f, 20f, Tint(e.color), additive = true)
    case FxKind.Spark =>
      streaks += new Streak(e.x, e.y, e.x2, e.y2, Tint(e.color), 0.22f)
      add(art.glow, e.x2, e.y2, 0f, 0f, 0.2f, 34f, 14f, Tint(e.color), additive = true)
    case FxKind.Ice | FxKind.Splash =>
      burst(10)((a, k) => add(art.mote, e.x, e.y, cos(a) * 80f * k, sin(a) * 80f * k, 0.6f, 9f, 3f, Color(0.85f, 0.96f, 1f, 0.85f)))
      add(art.ring, e.x, e.y, 0f, 0f, 0.6f, e.a, e.a * 4f, Color(0.85f, 0.96f, 1f, 0.8f))
    case FxKind.Leaf if e.x2 != 0f =>
      // Goods on their way through the air.
      val d = dist(e.x, e.y, e.x2, e.y2)
      val a = atan2(e.y2 - e.y, e.x2 - e.x)
      burst(6)((_, k) => add(art.mote, e.x + rng.range(-6f, 6f), e.y + rng.range(-6f, 6f), cos(a) * d / 0.45f, sin(a) * d / 0.45f, 0.4f + 0.1f * k, 8f, 4f, Tint(e.color), drag = 0f))
    case FxKind.Leaf =>
      add(art.leaf, e.x, e.y, rng.range(-20f, 20f), rng.range(-30f, 0f), 1.4f, 8f, 6f, Tint(e.color), spin = rng.range(-4f, 4f), aspect = 0.66f)
    case FxKind.Ring =>
      val c = Tint(e.color)
      add(art.ring, e.x, e.y, 0f, 0f, 1.1f, e.a * 0.3f, e.a * 2.4f, c, additive = true)
      add(art.glow, e.x, e.y, 0f, 0f, 0.9f, e.a * 2f, e.a * 0.8f, c.withAlpha(0.8f), additive = true)
      burst(30)((a, k) => add(art.mote, e.x, e.y, cos(a) * e.a * 2f * k, sin(a) * e.a * 2f * k, 1.2f, 9f, 2f, c, additive = true))
  }

  /** Sparks under the lamplighter's tools. */
  def weld(x: Float, y: Float, size: Float): Unit = if(rng.chance(0.5f)) {
    val a = rng.float() * Tau
    add(art.spark, x + rng.range(-size, size), y + rng.range(-size, size), cos(a) * 60f, sin(a) * 60f, 0.25f, 8f, 2f, Color(1f, 0.85f, 0.5f, 1f), additive = true, aspect = 0.35f, rotation = a)
  }
  def smoke(x: Float, y: Float, dark: Boolean): Unit = {
    val k = rng.float()
    add(art.puff, x + rng.range(-3f, 3f), y, 6f + rng.range(-4f, 4f), -14f, 1.5f + k, 7f, 22f, if(dark) Color(0.35f, 0.32f, 0.36f, 0.4f) else Color(0.95f, 0.95f, 0.95f, 0.35f), drag = 0.4f, rise = -6f)
  }
  def steam(x: Float, y: Float, color: Int): Unit =
    add(art.puff, x + rng.range(-6f, 6f), y + rng.range(-6f, 6f), 0f, -16f, 0.8f, 6f, 16f, Tint(color, 0.35f), drag = 0.5f)
  /** What a rocket or a flame leaves behind. */
  def trail(x: Float, y: Float, color: Int, size: Float): Unit =
    add(art.mote, x, y, rng.range(-6f, 6f), rng.range(-6f, 6f), 0.35f, size, 1f, Tint(color, 0.7f), additive = true, drag = 1f)
  def ember(x: Float, y: Float): Unit = add(art.mote, x, y, rng.range(-8f, 8f), -20f - rng.float() * 16f, 1.1f, 4.5f, 1.5f, Color(1f, 0.6f, 0.25f, 0.95f), additive = true, drag = 0.6f)

  def update(dt: Float): Unit = {
    var i = 0
    while(i < live.size) {
      val p = live(i)
      p.age += dt
      if(p.age >= p.life) {
        val last = live.remove(live.size - 1)
        if(!(last eq p)) live(i) = last
        pool += p
      } else {
        val k = scala.math.max(0f, 1f - p.drag * dt)
        p.vx *= k
        p.vy = p.vy * k + p.rise * dt
        p.x += p.vx * dt
        p.y += p.vy * dt
        p.rotation += p.spin * dt
        i += 1
      }
    }
    i = 0
    while(i < streaks.size) {
      streaks(i).age += dt
      if(streaks(i).age >= streaks(i).life) { val _ = streaks.remove(i) } else i += 1
    }
  }

  def draw(painter: Painter2D, x0: Float, y0: Float, x1: Float, y1: Float): Unit = {
    def pass(additive: Boolean): Unit = {
      var i = 0
      while(i < live.size) {
        val p = live(i)
        if(p.additive == additive && p.x > x0 - 60f && p.x < x1 + 60f && p.y > y0 - 60f && p.y < y1 + 60f) {
          val t = p.age / p.life
          val s = p.size0 + (p.size1 - p.size0) * t
          val fade = if(t < 0.5f) 1f else (1f - t) * 2f
          painter.sprite(p.texture, p.x, p.y, s, s * p.aspect, p.rotation, p.color.withAlpha(p.color.alpha * fade))
        }
        i += 1
      }
    }
    pass(additive = false)
    painter.additive {
      pass(additive = true)
      streaks.foreach { s =>
        val fade = 1f - s.age / s.life
        if(s.straight) {
          painter.line(s.x, s.y, s.x2, s.y2, 14f * fade, s.color.withAlpha(0.4f * fade))
          painter.line(s.x, s.y, s.x2, s.y2, 5f * fade, Color(1f, 1f, 1f, fade))
        } else {
        // A jagged line, drawn anew every frame.
        val n = 5
        var px = s.x
        var py = s.y
        val length = dist(s.x, s.y, s.x2, s.y2)
        var i = 1
        while(i <= n) {
          val t = i.toFloat / n
          val j = if(i == n) 0f else length * 0.09f
          val qx = s.x + (s.x2 - s.x) * t + rng.range(-j, j)
          val qy = s.y + (s.y2 - s.y) * t + rng.range(-j, j)
          painter.line(px, py, qx, qy, 5f * fade, s.color.withAlpha(0.45f * fade))
          painter.line(px, py, qx, qy, 2f, Color(1f, 1f, 1f, fade))
          px = qx
          py = qy
          i += 1
        }
        }
      }
    }
  }
}
