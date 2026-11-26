package hearthwheel

import java.nio.file.Paths

import hearthwheel.core.{PlayScene, Showcase, TitleScene}
import hearthwheel.model._

/** Draws the same works in each season, without the interface, for
  * tools/Banner.java: `banner <directory> [season...]`.
  */
object Banner {
  def main(args: Array[String]): Unit = {
    val out = Paths.get(args(0))
    val only = args.drop(1).map(_.toLowerCase).toSet
    val game = new Headless(TitleScene, 1280, 640)
    game.start()
    Season.values.filter(s => only.isEmpty || only.contains(s.label.toLowerCase)).foreach { s =>
      val rules = Valleys.all(1).copy(startSeason = s.ordinal, free = true, firstWave = 3f, threat = 3f)
      game.load(new PlayScene(() => Showcase.build(MapGen.generate(rules)), -1))
      game.step(3)
      game.play.hidden = true
      game.play.zoomGoal = 1.3f
      game.step(1500)
      game.play.messages.clear()
      game.step(2)
      game.save(out, "banner_" + s.label.toLowerCase)
      println(s.label)
    }
    game.stop()
  }
}
