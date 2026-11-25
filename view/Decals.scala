package hearthwheel
package core

import scala.collection.mutable

import sgl.engine.gameobject.render2d.{Effect2D, Painter2D, Texture2D}
import sgl.engine.gameobject.render3d.Renderer
import sgl.engine.gameobject.{Color, Scope}
import hearthwheel.model.Rng

/** Marks on the ground, which fade. */
final class Decals(art: Gallery) {
  private final class Mark(val texture: Texture2D, val x: Float, val y: Float, val size: Float, val turn: Float, val color: Color, val life: Float) { var age = 0f }
  private val marks = mutable.ArrayDeque.empty[Mark]
  private val rng = new Rng(5L)
  def scorch(x: Float, y: Float, r: Float): Unit =
    add(new Mark(art.scorch(rng.int(2)), x, y, r * 2.4f, rng.float() * 6.28f, Color(1f, 1f, 1f, 0.75f), 40f))
  def splat(x: Float, y: Float, r: Float, color: Int): Unit =
    add(new Mark(art.splat, x, y, r * 3f, rng.float() * 6.28f, Tint(color, 0.5f), 18f))
  private def add(m: Mark): Unit = {
    marks.append(m)
    if(marks.size > 260) { val _ = marks.removeHead() }
  }
  def update(dt: Float): Unit = {
    marks.foreach(_.age += dt)
    while(marks.nonEmpty && marks.head.age >= marks.head.life) { val _ = marks.removeHead() }
  }
  def draw(p: Painter2D, x0: Float, y0: Float, x1: Float, y1: Float): Unit = marks.foreach { m =>
    if(m.age < m.life && m.x > x0 - 80f && m.x < x1 + 80f && m.y > y0 - 80f && m.y < y1 + 80f) {
      val fade = scala.math.min(1f, (m.life - m.age) / 6f)
      p.sprite(m.texture, m.x, m.y, m.size, m.size, m.turn, m.color.withAlpha(m.color.alpha * fade))
    }
  }
}

/** The shaders of the game. */
final class Effects {
  var water: Effect2D = null
  var heat: Effect2D = null
  def create(renderer: Renderer)(using Scope): Unit = {
    // Light that wanders over the water.
    water = renderer.createEffect2D("""
      float wave(vec2 p, float t) {
        return sin(p.x * 0.061 + t * 1.1 + sin(p.y * 0.043 + t * 0.7) * 1.7) + sin(p.y * 0.077 - t * 0.9 + sin(p.x * 0.037 - t * 0.5) * 1.9);
      }
      vec4 effect(vec4 color, vec2 uv, vec2 position, vec4 params) {
        float w = wave(position, params.x) * 0.5 + wave(position * 1.9 + 40.0, params.x * 1.3) * 0.3;
        float line = smoothstep(0.62, 0.98, 1.0 - abs(w));
        return vec4(1.0, 1.0, 1.0, color.a * line * params.y);
      }
    """)
    // A dome, brighter where it was struck and at its edge.
    heat = renderer.createEffect2D("""
      vec4 effect(vec4 color, vec2 uv, vec2 position, vec4 params) {
        vec2 d = (position - params.xy) / max(1.0, params.z);
        float r = length(d);
        float edge = smoothstep(0.55, 1.0, r);
        float bands = 0.5 + 0.5 * sin(r * 26.0 - params.w * 5.0);
        return vec4(color.rgb, color.a * (0.35 + edge * 0.9 + bands * 0.12));
      }
    """)
  }
}
