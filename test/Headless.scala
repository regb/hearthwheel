package hearthwheel

import java.nio.file.{Files, Path}

import _root_.sgl.{CapturedFrame, DesktopSystemProvider, Input, MemorySaveComponent}
import _root_.sgl.desktop.DesktopFrameIO
import _root_.sgl.engine.gameobject._
import _root_.sgl.headless.opengl.HeadlessOpenGLApp
import _root_.sgl.util.NoLoggingProvider

/** The game without a window, driven by hand. */
/** @param fancy with bloom and the rest, which take long to draw without a GPU */
final class Headless(scene: Scene, width: Int = 1280, height: Int = 720, fancy: Boolean = true) {
  private var context: GameContext = null

  private object app extends HeadlessOpenGLApp
      with core.AbstractApp
      with core.GeneratedAssetsProvider
      with DesktopSystemProvider
      with MemorySaveComponent
      with NoLoggingProvider {
    override val frameDimension: (Int, Int) = (width, height)
    override protected def firstScene: Scene = new Scene {
      override def load(loaded: GameContext)(using Scope): Unit = {
        context = loaded
        if(!fancy) {
          val profile = loaded.global[core.Profile]
          profile.bloom = false
          profile.clouds = false
          profile.weather = false
          profile.ripples = false
        }
        loaded.runtime.loadScene(scene)
      }
    }
    override protected def newSeed: Long = 5L
  }
  private def controller = app.Controller
  def world: World = context.world
  def play: core.Play = world.globals.get[core.Play]
  def inValley: Boolean = world.globals.getOption[core.Play].nonEmpty
  def load(next: Scene): Unit = context.runtime.loadScene(next)

  def start(): Unit = {
    app.start()
    // Until the pictures are there, and the first scene with them.
    val deadline = java.lang.System.nanoTime() + 60_000_000_000L
    while(context == null && java.lang.System.nanoTime() < deadline) {
      Thread.sleep(10)
      step()
    }
    step(3)
  }
  def stop(): Unit = app.stop()
  def step(n: Int = 1): Unit = for(_ <- 0 until n) app.step(1.0 / 60.0)

  def key(k: Input.Keys.Key): Unit = {
    controller.keyDown(k)
    step()
    controller.keyUp(k)
    step()
  }
  def type_(text: String): Unit = text.foreach { c => controller.textInput(c.toString); step() }
  def controllerDown(k: Input.Keys.Key): Unit = controller.keyDown(k)
  def controllerUp(k: Input.Keys.Key): Unit = controller.keyUp(k)
  /** Holds the left button down for some frames. */
  def hold(x: Float, y: Float, frames: Int): Unit = {
    move(x, y)
    controller.mouseDown(x.toInt, y.toInt, Input.MouseButtons.Left)
    step(frames)
    controller.mouseUp(x.toInt, y.toInt, Input.MouseButtons.Left)
    step()
  }
  def move(x: Float, y: Float): Unit = { controller.mouseMoved(x.toInt, y.toInt); step() }
  def drag(x0: Float, y0: Float, x1: Float, y1: Float, button: Input.MouseButtons.MouseButton = Input.MouseButtons.Left): Unit = {
    move(x0, y0)
    controller.mouseDown(x0.toInt, y0.toInt, button)
    step(2)
    for(i <- 1 to 8) move(x0 + (x1 - x0) * i / 8f, y0 + (y1 - y0) * i / 8f)
    controller.mouseUp(x1.toInt, y1.toInt, button)
    step(2)
  }
  def click(x: Float, y: Float, button: Input.MouseButtons.MouseButton = Input.MouseButtons.Left): Unit = {
    move(x, y)
    controller.mouseDown(x.toInt, y.toInt, button)
    step(2)
    controller.mouseUp(x.toInt, y.toInt, button)
    step(2)
  }

  def frame(): CapturedFrame = {
    val pending = app.captureNextFrame()
    step()
    pending.value.flatMap(_.toOption).getOrElse(throw new IllegalStateException("No frame was captured"))
  }
  def save(directory: Path, name: String): Unit = {
    Files.createDirectories(directory)
    DesktopFrameIO.writePng(frame(), directory.resolve(name + ".png"))
  }
}
