package hearthwheel
package core

import scala.collection.mutable

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.render2d.{Painter2D, Text2D, Texture2D}
import sgl.geometry.Vec2

/** An immediate mode user interface: each frame draws its widgets, and asks
  * them what was done to them. Sizes are in points, which are scaled to pixels.
  */
final class Ui(val art: Gallery, click: () => Unit) {
  private var painter: Painter2D = null
  /** Pixels to a point. */
  var scale = 1f
  var width = 1280f
  var height = 720f
  var mouseX = 0f
  var mouseY = 0f
  var time = 0f

  private final case class Box(x: Float, y: Float, w: Float, h: Float) {
    def has(px: Float, py: Float): Boolean = px >= x && py >= y && px < x + w && py < y + h
  }
  // Where the interface was in the last frame, and where it is in this one.
  private var caught = Vector.empty[Box]
  private val catching = mutable.ArrayBuffer.empty[Box]
  private var pressed: Option[(Float, Float)] = None
  private var released: Option[(Float, Float)] = None
  private var wheelTurned = 0f
  private var down = false
  // When set, only what is drawn within it reacts: what is under a dialog does not.
  private var modal = false
  private var nextModal = false
  private var inModal = false

  /** Whether the interface is at that pixel, and not the world. */
  def catches(px: Float, py: Float): Boolean = modal || caught.exists(_.has(px / scale, py / scale))
  def press(px: Float, py: Float): Unit = { pressed = Some((px / scale, py / scale)); down = true }
  def release(px: Float, py: Float): Unit = { released = Some((px / scale, py / scale)); down = false }
  def wheel(notches: Float): Unit = wheelTurned += notches
  def isDown: Boolean = down

  def begin(p: Painter2D, screen: Vec2, uiScale: Float, pointerX: Float, pointerY: Float, seconds: Float): Unit = {
    painter = p
    scale = uiScale
    width = screen.x / scale
    height = screen.y / scale
    mouseX = pointerX / scale
    mouseY = pointerY / scale
    time += seconds
    catching.clear()
    inModal = false
  }

  def end(): Unit = {
    caught = catching.toVector
    if(released.nonEmpty) { released = None; pressed = None }
    wheelTurned = 0f
    modal = nextModal
    nextModal = false
    painter = null
  }

  /** From here on this is what reacts, over everything before it. */
  def beginModal(): Unit = {
    nextModal = true
    inModal = true
  }
  private def live: Boolean = !modal || inModal

  def catchMouse(x: Float, y: Float, w: Float, h: Float): Unit = catching += Box(x, y, w, h)
  def over(x: Float, y: Float, w: Float, h: Float): Boolean = live && Box(x, y, w, h).has(mouseX, mouseY)
  def clicked(x: Float, y: Float, w: Float, h: Float): Boolean = {
    val box = Box(x, y, w, h)
    live && released.exists { case (rx, ry) => box.has(rx, ry) } && pressed.exists { case (sx, sy) => box.has(sx, sy) }
  }
  /** The pointer, while it is held down after being pressed in there. */
  def dragged(x: Float, y: Float, w: Float, h: Float): Option[(Float, Float)] =
    if(live && down && pressed.exists { case (sx, sy) => Box(x, y, w, h).has(sx, sy) }) Some((mouseX, mouseY)) else None
  def turned(x: Float, y: Float, w: Float, h: Float): Float = if(over(x, y, w, h)) wheelTurned else 0f

  // -------------------------------------------------------------- drawing

  def rect(x: Float, y: Float, w: Float, h: Float, color: Color): Unit = painter.rect(x * scale, y * scale, w * scale, h * scale, color)
  def line(x0: Float, y0: Float, x1: Float, y1: Float, thickness: Float, color: Color): Unit =
    painter.line(x0 * scale, y0 * scale, x1 * scale, y1 * scale, thickness * scale, color)
  def picture(t: Texture2D, x: Float, y: Float, w: Float, h: Float, color: Color = Color.White, rotation: Float = 0f): Unit =
    painter.sprite(t, (x + w / 2f) * scale, (y + h / 2f) * scale, w * scale, h * scale, rotation, color)
  def clip(x: Float, y: Float, w: Float, h: Float)(draw: => Unit): Unit = painter.clip(Vec2(x * scale, y * scale), Vec2(w * scale, h * scale))(draw)
  def raw: Painter2D = painter
  def additive(draw: => Unit): Unit = painter.additive(draw)

  def nine(t: Texture2D, x: Float, y: Float, w: Float, h: Float, border: Float = 12f, color: Color = Color.White): Unit = {
    val u = 16f / 64f
    val b = scala.math.min(border, scala.math.min(w, h) / 2f)
    val xs = Array(x, x + b, x + w - b, x + w)
    val ys = Array(y, y + b, y + h - b, y + h)
    val us = Array(0f, u, 1f - u, 1f)
    var j = 0
    while(j < 3) {
      var i = 0
      while(i < 3) {
        val pw = xs(i + 1) - xs(i)
        val ph = ys(j + 1) - ys(j)
        if(pw > 0f && ph > 0f)
          painter.sprite(t, (xs(i) + pw / 2f) * scale, (ys(j) + ph / 2f) * scale, pw * scale, ph * scale, 0f, color, us(i), us(j), us(i + 1), us(j + 1))
        i += 1
      }
      j += 1
    }
  }

  def panel(x: Float, y: Float, w: Float, h: Float, alpha: Float = 1f): Unit = {
    catchMouse(x, y, w, h)
    rect(x + 4f, y + 5f, w - 4f, h - 3f, Color(0f, 0f, 0f, 0.22f * alpha))
    nine(art("panel"), x, y, w, h, color = Tint.alpha(alpha))
  }
  def paper(x: Float, y: Float, w: Float, h: Float, catches: Boolean = true): Unit = {
    if(catches) catchMouse(x, y, w, h)
    rect(x + 4f, y + 5f, w - 4f, h - 3f, Color(0f, 0f, 0f, 0.25f))
    nine(art("paper"), x, y, w, h)
  }

  def textWidth(s: String, size: Float, bold: Boolean = false): Float = Text2D.width(Lang(s), size, bold)
  def text(s: String, x: Float, y: Float, size: Float, color: Color = Tint.Cream, bold: Boolean = false): Unit =
    say(Lang(s), x, y, size, color, bold)
  private def say(s: String, x: Float, y: Float, size: Float, color: Color, bold: Boolean): Unit =
    painter.text(s, Vec2(x * scale, y * scale), size * scale, color, bold)
  def centered(s: String, cx: Float, y: Float, size: Float, color: Color = Tint.Cream, bold: Boolean = false): Unit =
    text(s, cx - textWidth(s, size, bold) / 2f, y, size, color, bold)
  def right(s: String, rx: Float, y: Float, size: Float, color: Color = Tint.Cream, bold: Boolean = false): Unit =
    text(s, rx - textWidth(s, size, bold), y, size, color, bold)
  /** Text with a dark edge, to be read over the world. */
  def outlined(s: String, x: Float, y: Float, size: Float, color: Color = Tint.Cream, bold: Boolean = true): Unit = {
    val edge = Tint.Ink.withAlpha(0.8f * color.alpha)
    text(s, x + 1f, y + 1f, size, edge, bold)
    text(s, x - 1f, y + 1f, size, edge, bold)
    text(s, x, y - 1f, size, edge, bold)
    text(s, x, y, size, color, bold)
  }
  def wrapped(s: String, x: Float, y: Float, w: Float, size: Float, color: Color = Tint.Cream, bold: Boolean = false): Float = {
    var at = y
    Text2D.wrap(Lang(s), w, size, bold).foreach { l =>
      say(l, x, at, size, color, bold)
      at += Text2D.lineHeight(size)
    }
    at
  }
  def linesOf(s: String, w: Float, size: Float): Int = Text2D.wrap(Lang(s), w, size).size
  def lineHeight(size: Float): Float = Text2D.lineHeight(size)

  def bar(x: Float, y: Float, w: Float, h: Float, share: Float, color: Color): Unit = {
    rect(x, y, w, h, Tint.Ink.withAlpha(0.85f))
    rect(x + 1f, y + 1f, (w - 2f) * scala.math.max(0f, scala.math.min(1f, share)), h - 2f, color)
  }

  // -------------------------------------------------------------- widgets

  def button(x: Float, y: Float, w: Float, h: Float, label: String, enabled: Boolean = true, size: Float = 16f, icon: Texture2D = null): Boolean = {
    catchMouse(x, y, w, h)
    val hover = enabled && over(x, y, w, h)
    val held = hover && down
    nine(if(!enabled) art("button_off") else if(hover) art("button_hot") else art("button"), x, y, w, h)
    val dy = if(held) 1.5f else 0f
    val color = if(enabled) Tint.Ink else Tint(0x9a8a84)
    if(icon != null && label.isEmpty) {
      val s = scala.math.min(w, h) * 0.62f
      picture(icon, x + (w - s) / 2f, y + (h - s) / 2f + dy, s, s, if(enabled) Color.White else Tint.alpha(0.5f))
    } else if(icon != null) {
      val s = h * 0.6f
      picture(icon, x + 10f, y + (h - s) / 2f + dy, s, s, if(enabled) Color.White else Tint.alpha(0.5f))
      val fit = fitting(label, w - 22f - s, size)
      text(label, x + 16f + s, y + (h - lineHeight(fit)) / 2f + dy, fit, color, bold = true)
    } else {
      val fit = fitting(label, w - 12f, size)
      centered(label, x + w / 2f, y + (h - lineHeight(fit)) / 2f + dy, fit, color, bold = true)
    }
    val done = enabled && clicked(x, y, w, h)
    if(done) click()
    done
  }

  /** The size at which a label fits a width, at most the one asked for. */
  def fitting(label: String, w: Float, size: Float, bold: Boolean = true): Float = {
    val wide = textWidth(label, size, bold)
    if(wide <= w || wide <= 0f) size else scala.math.max(size * 0.55f, size * w / wide)
  }

  def slot(x: Float, y: Float, s: Float, hot: Boolean): Boolean = {
    catchMouse(x, y, s, s)
    nine(if(hot || over(x, y, s, s)) art("slot_hot") else art("slot"), x, y, s, s, 10f)
    val done = clicked(x, y, s, s)
    if(done) click()
    done
  }

  /** A slider from 0 to 1; returns its value. */
  def slider(x: Float, y: Float, w: Float, label: String, value: Float): Float = {
    text(label, x, y, 15f)
    val track = y + 24f
    catchMouse(x, track - 4f, w, 22f)
    rect(x, track + 4f, w, 6f, Tint.Ink)
    rect(x, track + 4f, w * value, 6f, Tint.Gold)
    val now = dragged(x - 8f, track - 6f, w + 16f, 26f).map { case (mx, _) => scala.math.max(0f, scala.math.min(1f, (mx - x) / w)) }.getOrElse(value)
    nine(art("button_hot"), x + w * now - 8f, track - 2f, 16f, 18f, 6f)
    right((now * 100f).toInt.toString + "%", x + w, y, 15f, Tint.Soft)
    now
  }

  def toggle(x: Float, y: Float, w: Float, label: String, value: Boolean): Boolean = {
    catchMouse(x, y, w, 26f)
    nine(art(if(over(x, y, w, 26f)) "slot_hot" else "slot"), x, y, 26f, 26f, 8f)
    if(value) picture(art.icon("check"), x + 4f, y + 4f, 18f, 18f)
    text(label, x + 36f, y + 3f, fitting(label, w - 38f, 15f, bold = false))
    if(clicked(x, y, w, 26f)) { click(); !value } else value
  }
}
