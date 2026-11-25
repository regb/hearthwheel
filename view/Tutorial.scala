package hearthwheel
package core

import hearthwheel.model._

/** The first steps in the first valley. Each is done once its condition holds. */
final class Tutorial(play: Play) {
  private final case class Step(title: String, text: String, done: () => Boolean, touch: String = "")
  private val w = play.world
  private def has(b: Block, n: Int = 1): Boolean = w.buildings.count(x => x.alive && (x.block eq b)) >= n
  private val startX = w.lantern.x
  private val startY = w.lantern.y
  private var startedLearning = false
  private val steps = Vector(
    Step("Wake up", "Fly the lamplighter with W A S D or the arrow keys. The wheel zooms.", () => w.lantern != null && MathUtil.dist(w.lantern.x, w.lantern.y, startX, startY) > 120f,
      "Drag over the land: the lamplighter goes where you look. Two fingers zoom."),
    Step("Dig", "Choose the Wheel Drill under Gathering (bottom right) and place two on copper, the orange seam.", () => has(Blocks.Drill, 2)),
    Step("Haul", "Drag a line of Belts from the drills to the Hearth. A belt carries what drills beside it dig.", () => w.deliveredCounts(Items.Copper.id) >= 5),
    Step("More of everything", "Bring in tin, flint and coal the same way. R turns what you hold, right click lets go of it, and dragging with the right button takes things down.", () => Vector(Items.Tin, Items.Flint, Items.Coal).count(i => w.deliveredCounts(i.id) >= 3) >= 2,
      "Bring in tin, flint and coal the same way. The buttons at the bottom left turn what you hold, or let go of it. The red cross takes things down."),
    Step("Guard", "The Bramble comes out of the Briar Heart, on the minimap. Put Slingers in its way and lead a belt of copper or flint past them.", () => w.buildings.exists { case t: TurretBuilding => t.hasAmmo && t.role.ammo.nonEmpty; case _ => false }),
    Step("Learn", "Press T to open the almanac. Pages are paid for out of the Hearth. Start with the Smelter.", () => { if(w.learned.size > Pages.basics.size) startedLearning = true; startedLearning },
      "Open the almanac, the book at the top right. Pages are paid for out of the Hearth. Start with the Smelter."),
    Step("The year", "The dial at the top shows the season. In summer the shallows dry out and can be built on; in winter everything freezes, and machines want warmth.", () => w.seasonIndex != 0 || w.seasonTime > w.seasonLength * 0.9f),
    Step("Before winter", "Learn the Brazier, and feed some coal to one beside your turrets. Machines in the cold run at half speed.", () => has(Blocks.Brazier)),
    Step("Hold on", "Keep the Hearth lit until spring comes again.", () => w.year > 1)
  )
  private var index = 0
  private var celebrate = 0f

  def update(dt: Float): Unit = if(index < steps.size) {
    celebrate = scala.math.max(0f, celebrate - dt)
    if(steps(index).done()) {
      index += 1
      celebrate = 1f
      play.sound(SoundKind.Learn, 0.4f)
    }
  }
  def title: Option[String] = steps.lift(index).map(_.title)
  def text: String = steps.lift(index).map(s => if(play.controls.touch && s.touch.nonEmpty) s.touch else s.text).getOrElse("")
  def progress: String = s"${index + 1} of ${steps.size}"
  def flash: Float = celebrate
}
