package hearthwheel
package core

import scala.collection.mutable

import sgl.{InputActions, InputProcessor}
import sgl.Input.{Keys, MouseButtons, ScrollUnits}

/** What a player did, whatever they did it with. Points are in pixels. */
enum Act {
  case Press(x: Float, y: Float, button: Int)
  case Drag(x: Float, y: Float)
  case Release(x: Float, y: Float, button: Int)
  case Wheel(notches: Float, x: Float, y: Float)
  /** Two fingers: how far the middle between them went, and how much wider they are. */
  case Pinch(dx: Float, dy: Float, factor: Float, x: Float, y: Float)
  case Key(key: Keys.Key)
  case KeyUp(key: Keys.Key)
  case Text(text: String)
  case Back
}

/** Collects input events between frames. */
final class Controls extends InputProcessor {
  private val queue = mutable.ArrayBuffer.empty[Act]
  var mouseX = 0f
  var mouseY = 0f
  /** Whether the last input came from fingers. */
  var touch = false
  private val held = mutable.HashSet.empty[Keys.Key]
  private val fingers = mutable.LinkedHashMap.empty[Int, (Float, Float)]
  // Once a second finger is down, the first one is no longer a press.
  private var pinching = false

  def isHeld(key: Keys.Key): Boolean = held.contains(key)
  def shift: Boolean = isHeld(Keys.LeftShift) || isHeld(Keys.RightShift)

  def take(): Vector[Act] = {
    val result = queue.toVector
    queue.clear()
    result
  }

  private def button(b: MouseButtons.MouseButton): Int = b match {
    case MouseButtons.Left => 0
    case MouseButtons.Right => 1
    case MouseButtons.Middle => 2
    case MouseButtons.Other(_) => -1
  }
  private def at(x: Int, y: Int): Unit = { mouseX = x.toFloat; mouseY = y.toFloat }

  override def mouseMoved(x: Int, y: Int): Boolean = { touch = false; at(x, y); queue += Act.Drag(mouseX, mouseY); true }
  override def mouseDown(x: Int, y: Int, b: MouseButtons.MouseButton): Boolean = { touch = false; at(x, y); if(button(b) >= 0) queue += Act.Press(mouseX, mouseY, button(b)); true }
  override def mouseUp(x: Int, y: Int, b: MouseButtons.MouseButton): Boolean = { at(x, y); if(button(b) >= 0) queue += Act.Release(mouseX, mouseY, button(b)); true }
  override def mouseScrolled(amountX: Float, amountY: Float, unit: ScrollUnits.ScrollUnit): Boolean = {
    // In notches of a wheel.
    val notches = unit match {
      case ScrollUnits.Notches | ScrollUnits.Pages => amountY
      case ScrollUnits.Pixels => amountY / 100f
      case ScrollUnits.Lines => amountY / 3f
    }
    queue += Act.Wheel(notches, mouseX, mouseY)
    true
  }

  override def touchDown(x: Int, y: Int, pointer: Int): Boolean = {
    touch = true
    fingers(pointer) = (x.toFloat, y.toFloat)
    if(fingers.size == 1) {
      at(x, y)
      queue += Act.Press(mouseX, mouseY, 0)
    } else if(!pinching) {
      pinching = true
      // Far away from everything, so that nothing is tapped.
      queue += Act.Release(-10000f, -10000f, 0)
    }
    true
  }
  override def touchMoved(x: Int, y: Int, pointer: Int): Boolean = {
    fingers.get(pointer).foreach { case (wasX, wasY) =>
      if(pinching && fingers.size >= 2) {
        fingers.iterator.find(_._1 != pointer).foreach { case (_, (ox, oy)) =>
          val before = MathUtil2.distance(wasX, wasY, ox, oy)
          val now = MathUtil2.distance(x.toFloat, y.toFloat, ox, oy)
          if(before > 4f && now > 4f)
            queue += Act.Pinch((x - wasX) / 2f, (y - wasY) / 2f, now / before, (x + ox) / 2f, (y + oy) / 2f)
        }
      } else if(!pinching) {
        at(x, y)
        queue += Act.Drag(mouseX, mouseY)
      }
      fingers(pointer) = (x.toFloat, y.toFloat)
    }
    true
  }
  override def touchUp(x: Int, y: Int, pointer: Int): Boolean = {
    if(fingers.remove(pointer).isDefined) {
      if(!pinching) { at(x, y); queue += Act.Release(mouseX, mouseY, 0) }
      if(fingers.isEmpty) pinching = false
    }
    true
  }

  override def keyDown(key: Keys.Key): Boolean = { held += key; queue += Act.Key(key); true }
  override def keyUp(key: Keys.Key): Boolean = { held -= key; queue += Act.KeyUp(key); true }
  override def textInput(text: String): Boolean = { queue += Act.Text(text); true }
  override def keyRepeated(key: Keys.Key): Boolean = { if(key == Keys.Backspace) queue += Act.Key(key); true }
  override def systemAction(action: InputActions.Action): Boolean = { queue += Act.Back; true }
}

private object MathUtil2 {
  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = scala.math.sqrt(((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toDouble).toFloat
}
