package hearthwheel
package core

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.render2d.Painter2D
import hearthwheel.model.MathUtil._

/** What falls and drifts through the air of each season. Nothing is kept: where
  * each speck is follows from the time and its number.
  */
final class Weather(art: Gallery) {
  private def hash(i: Int, salt: Int): Float = {
    var h = i * 374761393 + salt * 668265263
    h = (h ^ (h >>> 13)) * 1274126177
    ((h ^ (h >>> 16)) & 0xffff) / 65535f
  }
  private def wrap(v: Float, size: Float): Float = { val m = v % size; if(m < 0f) m + size else m }

  /** @param amount from 0 to 1, as a season comes and goes */
  def draw(p: Painter2D, season: Int, amount: Float, x0: Float, y0: Float, w: Float, h: Float, time: Float): Unit = if(amount > 0.01f) {
    val area = w * h
    val mw = w + 80f
    val mh = h + 80f
    def each(n: Int)(f: (Int, Float, Float) => Unit): Unit = {
      var i = 0
      while(i < n) { f(i, hash(i, 1), hash(i, 2)); i += 1 }
    }
    // Specks stay where they are in the world as the camera moves.
    def place(u: Float, vx: Float, origin: Float, size: Float): Float = origin - 40f + wrap(u * size + vx * time - origin, size)
    season match {
      case 0 =>
        each((area / 5200f * amount).toInt) { (_, a, b) =>
          p.sprite(art.drop, place(a, -60f, x0, mw), place(b, 620f, y0, mh), 2f, 15f, 0.1f, Color(0.75f, 0.9f, 1f, 0.3f))
        }
        each((area / 32000f * amount).toInt) { (i, a, b) =>
          p.sprite(art.petal, place(a, 14f, x0, mw) + sin(time * 1.3f + i) * 10f, place(b, 24f, y0, mh), 7f, 7f, time + i, Color(1f, 0.78f, 0.88f, 0.75f))
        }
      case 1 =>
        p.additive {
          each((area / 20000f * amount).toInt) { (i, a, b) =>
            val blink = 0.5f + 0.5f * sin(time * 1.1f + i * 2.4f)
            p.sprite(art.mote, place(a, 7f, x0, mw) + sin(time * 0.6f + i) * 14f, place(b, -9f, y0, mh), 5f, 5f, 0f, Color(1f, 0.92f, 0.55f, 0.4f * blink))
          }
        }
      case 2 =>
        each((area / 15000f * amount).toInt) { (i, a, b) =>
          val gust = sin(time * 0.5f + b * 4f) * 26f
          val c = if(i % 3 == 0) Tint(0xd8792f, 0.85f) else if(i % 3 == 1) Tint(0xc24f2c, 0.85f) else Tint(0xe8b23a, 0.85f)
          p.sprite(art.leaf, place(a, 130f + (i % 5) * 16f, x0, mw) + gust, place(b, 42f, y0, mh) + sin(time * 2f + i) * 12f, 9f, 6f, time * (1f + (i % 4)) + i, c)
        }
      case _ =>
        each((area / 4200f * amount).toInt) { (i, a, b) =>
          val s = 2.5f + (i % 4) * 1.1f
          val t = if(i % 5 == 0) art.flake else art.mote
          p.sprite(t, place(a, 22f + (i % 3) * 8f, x0, mw) + sin(time * 0.9f + i) * 10f, place(b, 40f + (i % 4) * 14f, y0, mh), s * 1.5f, s * 1.5f, time * 0.5f + i, Color(1f, 1f, 1f, 0.8f))
        }
    }
  }

  /** The shadows of clouds, going over. */
  def clouds(p: Painter2D, season: Int, x0: Float, y0: Float, w: Float, h: Float, time: Float): Unit = {
    val many = if(season == 1) 3 else if(season == 2) 7 else 5
    val span = 2400f
    var i = 0
    while(i < many) {
      val s = 620f + hash(i, 7) * 520f
      val x = x0 - span / 2f + wrap(hash(i, 5) * span + time * (14f + (if(season == 2) 26f else 0f)) - x0, span)
      val y = y0 - span / 2f + wrap(hash(i, 6) * span + time * 5f - y0, span)
      p.sprite(art.cloud, x + w * 0.3f, y + h * 0.3f, s, s * 0.7f, 0f, Color(1f, 1f, 1f, 0.36f))
      i += 1
    }
  }

  /** The light of a season, over everything. */
  def ambience(season: Int): Color = season match {
    case 0 => Color(0.55f, 0.85f, 0.75f, 0.05f)
    case 1 => Color(1f, 0.82f, 0.42f, 0.08f)
    case 2 => Color(0.85f, 0.42f, 0.18f, 0.10f)
    case _ => Color(0.16f, 0.24f, 0.5f, 0.24f)
  }
  /** How much fires stand out. */
  def glow(season: Int): Float = season match {
    case 0 => 0.35f
    case 1 => 0.2f
    case 2 => 0.6f
    case _ => 1.1f
  }
}
