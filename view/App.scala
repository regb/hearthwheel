package hearthwheel
package core

import scala.collection.mutable

import sgl.{AudioProvider, SaveComponent}
import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d.{Render2DModule, Texture2D}
import hearthwheel.model._
import sgl.geometry.Vec2
import sgl.util.Loader

/** What scenes need of the platform. */
trait Services {
  def screen: Vec2
  /** Pixels to a point of a screen at its natural size. */
  def density: Float
  def loadGallery(context: GameContext)(using Scope): Loader[Gallery]
  def play(sound: SoundKind, volume: Float, rate: Float): Unit
  /** Fades to a piece of music: by season, or 4 for battle. -1 for silence. */
  def music(piece: Int): Unit
  /** What the air sounds like, by season; -1 for silence. */
  def air(season: Int): Unit
  def updateAudio(dt: Float, musicVolume: Float, airVolume: Float): Unit
  def read(name: String): Option[String]
  def write(name: String, value: String): Unit
  def seed(): Long
  def now(): Long
  def exit(): Unit
  def canFullscreen: Boolean
  def fullscreen: Boolean
  def fullscreen_=(on: Boolean): Unit
  def copy(text: String): Unit
  /** Hands over what is on the clipboard, once it is there. */
  def paste(use: String => Unit): Unit
}

/** A factory and its defense, in valleys where the seasons turn. */
trait AbstractApp extends OpenGLGameObjectApp with AssetsProvider with Sheet {
  this: AudioProvider with SaveComponent =>

  /** The scene to open once everything is loaded. Replaced by tests. */
  protected def firstScene: Scene = TitleScene
  protected def newSeed: Long = System.currentTimeMillis

  private object services extends Services {
    private val sounds = mutable.Map.empty[SoundKind, Loader[Audio.Sound]]
    private var tracks: Vector[Loader[Audio.Music]] = Vector.empty
    // Five pieces of music, then four kinds of air.
    private val volumes = new Array[Float](9)
    private var wanted = -1
    private var wantedAir = -1
    private var lastMusic = -1f
    private var lastAir = -1f

    def startLoading(): Unit = {
      SoundKind.values.foreach { kind =>
        sounds(kind) = Audio.loadSound(kind match {
          case SoundKind.Sling => Assets.audio.sling
          case SoundKind.Lob => Assets.audio.lob
          case SoundKind.Bolt => Assets.audio.bolt
          case SoundKind.Zap => Assets.audio.zap
          case SoundKind.Beam => Assets.audio.beam
          case SoundKind.Spray => Assets.audio.spray
          case SoundKind.Boom => Assets.audio.boom
          case SoundKind.Sting => Assets.audio.sting
          case SoundKind.Spark => Assets.audio.spark
          case SoundKind.Chomp => Assets.audio.chomp
          case SoundKind.Built => Assets.audio.built
          case SoundKind.Place => Assets.audio.place
          case SoundKind.Break => Assets.audio.break
          case SoundKind.Season => Assets.audio.season
          case SoundKind.Wave => Assets.audio.wave
          case SoundKind.Alarm => Assets.audio.alarm
          case SoundKind.Learn => Assets.audio.learn
          case SoundKind.Click => Assets.audio.click
          case SoundKind.Deny => Assets.audio.deny
          case SoundKind.Win => Assets.audio.win
          case SoundKind.Lose => Assets.audio.lose
          case SoundKind.Ice => Assets.audio.ice
          case SoundKind.Death => Assets.audio.death
          case SoundKind.Rail => Assets.audio.rail
          case SoundKind.Rocket => Assets.audio.rocket
          case SoundKind.Flame => Assets.audio.flame
          case SoundKind.Dig => Assets.audio.dig
          case SoundKind.Deed => Assets.audio.deed
          case SoundKind.Horn => Assets.audio.horn
        })
      }
      tracks = Vector(Assets.audio.musicSpring, Assets.audio.musicSummer, Assets.audio.musicAutumn, Assets.audio.musicWinter, Assets.audio.musicBattle,
        Assets.audio.ambSpring, Assets.audio.ambSummer, Assets.audio.ambAutumn, Assets.audio.ambWinter).map { asset =>
        Audio.loadMusic(asset).map { m => m.setLooping(true); m }
      }
    }

    override def screen: Vec2 = Vec2(Window.width.toFloat, Window.height.toFloat)
    override def density: Float = scala.math.max(0.75f, Window.logicalPpi / 160f)

    override def loadGallery(context: GameContext)(using Scope): Loader[Gallery] = {
      val renderer = context.engine.renderer
      val textures = sheet.map { case (name, asset) => name -> renderer.loadTexture2D(asset) }
      Loader.combine(textures.map { case (_, t) => renderer.whenReady(t) }).map(_ => new Gallery(textures.toMap[String, Texture2D]))
    }

    // A sound that could not load is one that is not heard.
    override def play(sound: SoundKind, volume: Float, rate: Float): Unit =
      sounds.get(sound).flatMap(_.value).flatMap(_.toOption).foreach { loaded =>
        val _ = loaded.play(volume)
      }

    override def music(piece: Int): Unit = wanted = piece
    override def air(season: Int): Unit = wantedAir = season
    override def updateAudio(dt: Float, musicVolume: Float, airVolume: Float): Unit = {
      val changed = musicVolume != lastMusic || airVolume != lastAir
      lastMusic = musicVolume
      lastAir = airVolume
      tracks.zipWithIndex.foreach { case (loader, i) =>
        loader.value.flatMap(_.toOption).foreach { track =>
          val before = volumes(i)
          val goal = if(i == wanted || (i >= 5 && i - 5 == wantedAir)) 1f else 0f
          if(before != goal || changed) {
            val step = dt / 3f
            val now = if(goal > before) scala.math.min(goal, before + step) else scala.math.max(goal, before - step)
            volumes(i) = now
            track.setVolume(now * (if(i < 5) musicVolume else airVolume))
            if(before == 0f && now > 0f) track.play()
            if(before > 0f && now == 0f) track.stop()
          }
        }
      }
    }

    override def read(name: String): Option[String] = Save.getString(name).filter(_.nonEmpty)
    override def write(name: String, value: String): Unit = Save.putString(name, value)
    override def seed(): Long = newSeed
    override def now(): Long = System.currentTimeMillis
    override def exit(): Unit = System.exit()
    override def canFullscreen: Boolean = Window.canFullscreen
    override def fullscreen: Boolean = Window.isFullscreen
    override def fullscreen_=(on: Boolean): Unit = Window.setFullscreen(on)
    override def copy(text: String): Unit = Window.copyText(text)
    override def paste(use: String => Unit): Unit = Window.pasteText().onLoad(t => use(t.getOrElse("")))
  }

  override def configure(builder: GameBuilder): Unit = {
    services.startLoading()
    builder
      .install(Render2DModule)
      .setGlobal(ClearColor(Tint(0x1c1620)))
      .setGlobal(FixedDeltaTime(1.0 / 60.0, maxStepsPerFrame = 4))
      .setGlobal[Services](services)
      .setInitialScene(new BootScene(firstScene))
  }
}

/** Loads the pictures, which then stay. */
private final class BootScene(next: Scene) extends AsyncScene[Gallery] {
  override def preload(context: GameContext)(using Scope): Loader[Gallery] = context.global[Services].loadGallery(context)(using context.world.globalScope)
  override def load(context: GameContext, gallery: Gallery)(using Scope): Unit = {
    context.world.globals.set(gallery)
    context.world.globals.set(new Profile(context.global[Services]))
    context.runtime.loadScene(next)
  }
}
