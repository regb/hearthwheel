package hearthwheel
package core

import scala.collection.mutable

import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d.{Baked2D, Camera2D}
import sgl.engine.gameobject.spatial2d.{Spatial2D, Transform2D}
import hearthwheel.model._
import hearthwheel.model.MathUtil._
import sgl.geometry.Vec2

/** What the player is doing with the pointer. */
enum Tool {
  case Hand
  case Build(block: Block)
  case Remove
  case Copy
  /** A copied layout, as plans around (0, 0). */
  case Stamp(plans: Vector[Plan])
  case Command
  /** Puts up again what was destroyed in an area. */
  case Rebuild
}

enum Overlay { case None, Menu, Settings, Keys, Learn, Help, End, Codex, Map, Ledger, Layouts, Waves, Deeds, Saves, Loom }

/** A valley being played: what its systems share. */
final class Play(
    val world: Valley,
    val art: Gallery,
    val services: Services,
    val profile: Profile,
    val valley: Int,
    val camera: Camera2D,
    val cameraSpatial: Spatial2D,
    val terrain: Terrain,
    /** Nobody plays: the world is shown behind the title. */
    val attract: Boolean
) {
  private val T = Valley.Tile.toFloat
  val controls = new Controls
  val ui = new Ui(art, () => services.play(SoundKind.Click, 0.5f * profile.soundVolume, 1f))
  val particles = new Particles(art)
  val view = new WorldView(world, art, particles)
  val weather = new Weather(art)
  val puppets = mutable.HashMap.empty[Int, Puppet]
  var minimap: Baked2D = null

  var time = 0f
  var paused = false
  var speed = 1
  var overlay = Overlay.None
  /** Whether the end of the valley has been told. */
  var ended = false
  def running: Boolean = !paused && (overlay == Overlay.None || overlay == Overlay.End)

  // --------------------------------------------------------------- camera

  var centerX: Float = world.hearth.cx
  var centerY: Float = world.hearth.cy
  var zoom = 1.6f
  var zoomGoal = 1.6f
  /** Follows the lamplighter until the player looks elsewhere. */
  var following = true
  private var shakeAmount = 0f

  def viewWidth: Float = services.screen.x / zoom
  def viewHeight: Float = services.screen.y / zoom
  def left: Float = cameraSpatial.local.position.x - viewWidth / 2f
  def top: Float = cameraSpatial.local.position.y - viewHeight / 2f
  def sees(x: Float, y: Float, margin: Float): Boolean =
    x > left - margin && y > top - margin && x < left + viewWidth + margin && y < top + viewHeight + margin

  def toWorld(px: Float, py: Float): Vec2 = Vec2(left + px / zoom, top + py / zoom)
  def toScreen(x: Float, y: Float): Vec2 = Vec2((x - left) * zoom, (y - top) * zoom)
  def shake(amount: Float): Unit = if(profile.shake) shakeAmount = scala.math.min(9f, shakeAmount + amount)

  def zoomAt(factor: Float, px: Float, py: Float): Unit = {
    val goal = clamp(zoomGoal * factor, minZoom, 3.4f)
    if(!following) {
      // What is under the pointer stays there.
      val before = toWorld(px, py)
      centerX += (before.x - centerX) * (1f - zoomGoal / goal)
      centerY += (before.y - centerY) * (1f - zoomGoal / goal)
    }
    zoomGoal = goal
  }
  private def minZoom: Float = scala.math.max(0.45f, scala.math.max(services.screen.x / (world.width * T), services.screen.y / (world.height * T)))

  def updateCamera(dt: Float): Unit = {
    zoomGoal = clamp(zoomGoal, minZoom, 3.4f)
    zoom += (zoomGoal - zoom) * scala.math.min(1f, dt * 12f)
    val led = if(riding != null) riding else world.lantern
    if(following && led != null && !attract) {
      val k = scala.math.min(1f, dt * 5f)
      centerX += (led.x - centerX) * k
      centerY += (led.y - centerY) * k
    }
    val hw = scala.math.min(viewWidth / 2f, world.width * T / 2f)
    val hh = scala.math.min(viewHeight / 2f, world.height * T / 2f)
    centerX = clamp(centerX, hw, world.width * T - hw)
    centerY = clamp(centerY, hh, world.height * T - hh)
    shakeAmount = scala.math.max(0f, shakeAmount - dt * 14f)
    val sx = sin(time * 71f) * shakeAmount * 0.5f
    val sy = cos(time * 83f) * shakeAmount * 0.5f
    // On whole pixels, or tiles shimmer as the camera moves.
    val px = scala.math.round((centerX + sx) * zoom) / zoom
    val py = scala.math.round((centerY + sy) * zoom) / zoom
    cameraSpatial.local = Transform2D(Vec2(px, py))
    camera.viewportSize = Vec2(viewWidth, viewHeight)
  }

  // ---------------------------------------------------------------- tools

  var tool: Tool = Tool.Hand
  var category: Category = Category.Haul
  var rotation = 0
  /** The building whose panel is open. */
  var selected: Building = null
  /** Where the drag that is under way began, as a tile; null when there is none. */
  var dragStart: (Int, Int) = null
  var dragButton = 0
  var dragPixels = 0f
  var pressX = 0f
  var pressY = 0f
  var panning = false
  var shooting = false
  var lastX = 0f
  var lastY = 0f
  /** Whether the line goes sideways first, decided by how the drag began. */
  var horizontalFirst = true
  /** The config of the last zipline placed, to join the next one to it. */
  var lastZipline: (Int, Int) = null

  def pointer: Vec2 = toWorld(controls.mouseX, controls.mouseY)
  def pointerTile: (Int, Int) = { val p = pointer; (floor(p.x / T), floor(p.y / T)) }
  def overUi: Boolean = ui.catches(controls.mouseX, controls.mouseY)

  /** The top left tile for a block held by the pointer at its middle. */
  def originFor(block: Block, tx: Int, ty: Int): (Int, Int) = (tx - (block.size - 1) / 2, ty - (block.size - 1) / 2)

  def sound(kind: SoundKind, volume: Float = 0.6f): Unit = services.play(kind, volume * profile.soundVolume, 1f)

  /** The command that is being given another key. */
  var binding: Option[Command] = None
  /** The automaton being ridden, if any. */
  var riding: Creature = null
  /** Automata by the number they were given. */
  val groups: Array[Set[Int]] = Array.fill(10)(Set.empty[Int])
  /** Nothing of the interface is drawn. */
  var hidden = false
  /** Whether a drag finds its own way around things. */
  def seeking: Boolean = controls.shift
  /** What to look at in the codex. */
  var codexBlock: Block = null

  final class Toast(val title: String, val text: String, val icon: String) { var age = 0f }
  val toasts = mutable.ArrayBuffer.empty[Toast]
  def toast(title: String, text: String, icon: String): Unit = toasts += new Toast(title, text, icon)
  def toasting: Boolean = toasts.nonEmpty
  val decals = new Decals(art)
  val effects: Effects = new Effects

  // ------------------------------------------------------------- messages

  final class Message(val text: String, val color: Color) { var age = 0f }
  val messages = mutable.ArrayBuffer.empty[Message]
  def say(text: String, color: Color = Tint.Cream): Unit = {
    if(!messages.lastOption.exists(m => m.text == text && m.age < 1.5f)) messages += new Message(text, color)
    if(messages.size > 5) { val _ = messages.remove(0) }
  }

  /** Banner across the screen when a season or a wave comes. */
  var banner = ""
  var bannerIcon = -1
  var bannerAge = 10f

  val tutorial: Tutorial = if(world.rules.tutorial && !attract) new Tutorial(this) else null

  // Frames, for the statistics.
  var fps = 60f
  var simMillis = 0f

  def saveWorld(): Unit = if(!attract && world.outcome == Outcome.Playing) profile.storeWorld(world.rules.name, Codec.save(world))
}
