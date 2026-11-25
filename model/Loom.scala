package hearthwheel.model

import scala.collection.mutable
import scala.util.Try
import MathUtil._

/** What a card of a loom looks at. `pick` says what else it needs to know. */
enum Sense(val label: String, val pick: Pick) {
  case Always extends Sense("Always", Pick.None)
  case Goods extends Sense("The Hearth's stock of", Pick.Item)
  case Held extends Sense("Goods inside", Pick.Link)
  case Health extends Sense("The health, in %, of", Pick.Link)
  case Power extends Sense("Power stored here, in %", Pick.None)
  case Season extends Sense("The season, 1 is spring", Pick.None)
  case TurnIn extends Sense("Seconds until the season turns", Pick.None)
  case WaveIn extends Sense("Seconds until the next wave", Pick.None)
  case Foes extends Sense("Foes within 20 tiles", Pick.None)
  case Hearth extends Sense("The Hearth's health, in %", Pick.None)
  case Drum extends Sense("Drum", Pick.Drum)
}

/** What a card of a loom does. */
enum Doing(val label: String, val pick: Pick) {
  case Stop extends Doing("stop", Pick.Link)
  case Choose extends Doing("set to the item", Pick.LinkItem)
  case Put extends Doing("set drum", Pick.DrumValue)
  case Add extends Doing("add to drum", Pick.DrumValue)
  case Copy extends Doing("write it to drum", Pick.Drum)
  case Ring extends Doing("ring the horn", Pick.None)
  case Call extends Doing("call the next wave", Pick.None)
}

/** What has to be chosen along with it. */
enum Pick { case None, Item, Link, LinkItem, Drum, DrumValue }

/** If what is seen compares like that to a number, something is done. */
final case class Card(sense: Sense, of: Int, compare: Int, value: Int, doing: Doing, on: Int, amount: Int)

object Card {
  val Compares: Vector[String] = Vector("less than", "more than", "exactly")
  val blank: Card = Card(Sense.Foes, 0, 1, 0, Doing.Stop, 0, 0)
  val Most = 8

  def write(cards: Vector[Card]): String =
    cards.map(c => Vector(c.sense.ordinal, c.of, c.compare, c.value, c.doing.ordinal, c.on, c.amount).mkString(":")).mkString("!")
  def read(text: String): Vector[Card] = text.split('!').toVector.filter(_.nonEmpty).take(Most).flatMap { t =>
    Try {
      val Array(sense, of, compare, value, doing, on, amount) = t.split(':').map(_.toInt): @unchecked
      Card(Sense.fromOrdinal(sense), of, clamp(compare, 0, 2), value, Doing.fromOrdinal(doing), on, amount)
    }.toOption
  }
}

/** Reads punched cards, twice a second, and acts on what it is linked to. */
final class LoomBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val reach: Float)
    extends Building(world, block, x, y, rotation, team) {
  import Valley.Tile
  var cards: Vector[Card] = Vector.empty
  /** What it is linked to, as distances in tiles. What is rebuilt there is linked again. */
  val ties = mutable.ArrayBuffer.empty[(Int, Int)]
  val MostTies = 6
  private var look = 0f
  private var quiet = 0f
  /** The card that acted last, for the view. */
  var acted = -1
  var fired = 0f

  def tied(i: Int): Building = if(i < 0 || i >= ties.size) null else world.buildingAt(x + ties(i)._1, y + ties(i)._2) match {
    case null => null
    case b => if(b.team == team) b else null
  }
  def inReach(b: Building): Boolean = dist2(cx, cy, b.cx, b.cy) <= reach * Tile * reach * Tile
  /** Links a building, or lets it go; false when it cannot be. */
  def toggle(b: Building): Boolean = {
    val i = ties.indices.find(k => tied(k) eq b)
    if(i.nonEmpty) { val _ = ties.remove(i.get); true }
    else if(ties.size < MostTies && inReach(b) && !(b eq this)) { ties += ((b.x - x, b.y - y)); true }
    else false
  }

  def see(c: Card): Int = c.sense match {
    case Sense.Always => 0
    case Sense.Goods => world.hearthStock(Items.all(clamp(c.of, 0, Items.Count - 1)))
    case Sense.Held => tied(c.of) match { case null => 0; case b => if(b.stock == null) 0 else b.stock.total }
    case Sense.Health => tied(c.of) match { case null => 0; case b => (b.healthShare * 100f).toInt }
    case Sense.Power => if(power == null || power.capacity <= 0f) 0 else (power.stored / power.capacity * 100f).toInt
    case Sense.Season => world.seasonIndex + 1
    case Sense.TurnIn => if(world.longWinter) 9999 else world.secondsLeft.toInt
    case Sense.WaveIn => world.waves.countdown.toInt
    case Sense.Foes =>
      var n = 0
      world.eachCreature(cx, cy, 20f * Tile, 1 - team)(_ => n += 1)
      n
    case Sense.Hearth => if(world.hearth == null) 0 else (world.hearth.healthShare * 100f).toInt
    case Sense.Drum => world.drums(clamp(c.of, 0, world.drums.length - 1))
  }
  private def holds(c: Card, seen: Int): Boolean = (c.sense eq Sense.Always) || (c.compare match {
    case 0 => seen < c.value
    case 1 => seen > c.value
    case _ => seen == c.value
  })

  override def isPowered: Boolean = true
  override def update(dt: Float): Unit = {
    val running = powerShare > 0.3f && !stopped && !flooded
    ease(if(running && cards.nonEmpty) 1f else 0f, dt)
    spin += dt * activity
    fired = scala.math.max(0f, fired - dt * 2f)
    quiet -= dt
    look -= dt
    if(running && look <= 0f) {
      look = 0.5f
      cards.zipWithIndex.foreach { case (c, i) =>
        val seen = see(c)
        if(holds(c, seen)) { act(c, seen); acted = i; fired = 1f }
      }
    }
  }
  private def act(c: Card, seen: Int): Unit = c.doing match {
    case Doing.Stop => val b = tied(c.on); if(b != null) b.stoppedUntil = world.time + 0.7f
    case Doing.Choose => tied(c.on) match {
      case s: SorterBuilding => if(s.config != c.amount) s.configure(c.amount)
      case h: HatchBuilding => if(h.config != c.amount) h.configure(c.amount)
      case _ => ()
    }
    case Doing.Put => world.drum(c.on, c.amount)
    case Doing.Add => world.drum(c.on, world.drums(clamp(c.on, 0, world.drums.length - 1)) + c.amount)
    case Doing.Copy => world.drum(c.on, seen)
    case Doing.Ring => if(quiet <= 0f) {
      quiet = 20f
      world.notices += Notice("A loom rings the horn", 0xffd27a)
      world.sound(SoundKind.Horn, cx, cy)
    }
    case Doing.Call => if(quiet <= 0f && !world.rules.peaceful) { quiet = 5f; world.waves.launchNow() }
  }

  /** Its cards, and what it is linked to. */
  override def note: String = Card.write(cards) + "~" + ties.map { case (a, b) => s"$a:$b" }.mkString("!")
  override def renote(text: String): Unit = {
    val parts = text.split("~", 2)
    cards = Card.read(parts(0))
    ties.clear()
    if(parts.length > 1) parts(1).split('!').foreach { t =>
      Try { val Array(a, b) = t.split(':').map(_.toInt): @unchecked; (a, b) }.foreach(p => if(ties.size < MostTies) ties += p)
    }
  }
  override def write(out: Writer): Unit = out.word(note)
  override def read(in: Reader): Unit = renote(in.word())
}

/** Shows a drum, for all to read. */
final class SignBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  private var which = 0
  override def config: Int = which
  override def configure(value: Int): Unit = if(value >= 0) which = value % world.drums.length
  def shown: Int = world.drums(which)
  override def update(dt: Float): Unit = ()
  override def write(out: Writer): Unit = out.int(which)
  override def read(in: Reader): Unit = configure(in.int())
}
