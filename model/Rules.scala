package hearthwheel.model

enum Goal(val label: String) {
  case Survive(years: Int) extends Goal(if(years == 1) "Keep the Hearth lit for a year" else s"Keep the Hearth lit for $years years")
  case Cleanse extends Goal("Burn out every Briar Heart")
  case Wheel extends Goal("Complete the Great Wheel and outlast the Long Winter")
  case Endless extends Goal("Last as long as you can")
}

/** What sets one valley apart from another. */
final case class Rules(
    name: String,
    about: String,
    size: Int,
    seed: Long,
    goal: Goal,
    nests: Int,
    /** 1 is an even match. */
    threat: Float = 1f,
    toughness: Float = 1f,
    /** Seconds of spring, summer, autumn and winter. */
    seasons: Vector[Float] = Vector(150f, 140f, 140f, 120f),
    startSeason: Int = 0,
    waveSeconds: Float = 48f,
    firstWave: Float = 100f,
    water: Float = 0.5f,
    cliffs: Float = 0.5f,
    thickets: Float = 0.5f,
    ore: Float = 1f,
    start: Vector[Stack] = Vector(Stack(Items.Copper, 220), Stack(Items.Tin, 60), Stack(Items.Flint, 40), Stack(Items.Coal, 40)),
    /** Building costs nothing and everything is known. */
    free: Boolean = false,
    peaceful: Boolean = false,
    /** No cloudbursts, heatwaves, gales or blizzards. */
    calm: Boolean = false,
    buildSpeed: Float = 1f,
    /** The next wave waits for the last of the one before. */
    patient: Boolean = false,
    /** Waves come on their own. Otherwise only when they are called. */
    timer: Boolean = true,
    tutorial: Boolean = false,
    /** The Bramble spreads roots and pods out of its Hearts. */
    wild: Boolean = true,
    /** What to do on the way, one after the other, and what comes of it. */
    aims: Vector[(Aim, Vector[Stack])] = Vector.empty,
    /** A map made by hand, by its number; 0 for one out of the seed. */
    map: Int = 0
)

/** How hard the Bramble pushes, as the player chooses. */
enum Difficulty(val label: String, val threat: Float, val toughness: Float) {
  case Gentle extends Difficulty("Gentle", 0.65f, 0.85f)
  case Even extends Difficulty("Even", 1f, 1f)
  case Harsh extends Difficulty("Harsh", 1.35f, 1.15f)
  case Cruel extends Difficulty("Cruel", 1.8f, 1.35f)
}

object RulesCodec {
  def write(r: Rules, out: Writer): Unit = {
    out.int(r.name.length)
    r.name.foreach(c => out.int(c.toInt))
    out.int(r.size); out.long(r.seed)
    r.goal match {
      case Goal.Survive(y) => out.int(0); out.int(y)
      case Goal.Cleanse => out.int(1); out.int(0)
      case Goal.Wheel => out.int(2); out.int(0)
      case Goal.Endless => out.int(3); out.int(0)
    }
    out.int(r.nests); out.float(r.threat); out.float(r.toughness)
    r.seasons.foreach(out.float)
    out.int(r.startSeason); out.float(r.waveSeconds); out.float(r.firstWave)
    out.float(r.water); out.float(r.cliffs); out.float(r.thickets); out.float(r.ore)
    out.int(r.start.size)
    r.start.foreach { s => out.int(s.item.id); out.int(s.amount) }
    out.float(r.buildSpeed)
    Vector(r.free, r.peaceful, r.calm, r.patient, r.timer, r.tutorial, r.wild).foreach(b => out.int(if(b) 1 else 0))
    out.int(r.map)
    Aim.write(r.aims, out)
  }
  def read(in: Reader): Rules = {
    val name = (0 until in.int()).map(_ => in.int().toChar).mkString
    val (size, seed) = (in.int(), in.long())
    val (kind, years) = (in.int(), in.int())
    val goal = kind match {
      case 0 => Goal.Survive(years)
      case 1 => Goal.Cleanse
      case 2 => Goal.Wheel
      case _ => Goal.Endless
    }
    val (nests, threat, toughness) = (in.int(), in.float(), in.float())
    val seasons = Vector.fill(4)(in.float())
    val (startSeason, wave, first) = (in.int(), in.float(), in.float())
    val (water, cliffs, thickets, ore) = (in.float(), in.float(), in.float(), in.float())
    val start = Vector.fill(in.int())(Stack(Items.all(in.int()), in.int()))
    val speed = in.float()
    val flags = Vector.fill(7)(in.int() == 1)
    Rules(name, "", size, seed, goal, nests, threat, toughness, seasons, startSeason, wave, first, water, cliffs, thickets, ore, start,
      free = flags(0), peaceful = flags(1), calm = flags(2), buildSpeed = speed, patient = flags(3), timer = flags(4), tutorial = flags(5), wild = flags(6), map = in.int(), aims = Aim.read(in))
  }
}

/** A valley on the map of the region. Places are out of 100 by 100. */
final case class Place(rules: Rules, x: Float, y: Float, after: Vector[Int], needs: Vector[Block])

object Valleys {
  import Items._
  import Aim._
  import Blocks.{Smelter, Brazier, Boltcaster, Windmill, Gearworks, Almanac, EmberForge, GreatWheel, Radiator, Auger, Steelworks, Slipway, Toyworks, VentWell, SunLens, Aegis, WagonPost, Refit, MortarRow, Loom, GreatHearth, Pump, Bellows}
  private def to(a: Aim)(reward: (Item, Int)*): (Aim, Vector[Stack]) = (a, reward.toVector.map { case (i, n) => Stack(i, n) })
  private val hard = Vector(Stack(Copper, 400), Stack(Tin, 120), Stack(Flint, 100), Stack(Coal, 100))
  val places: Vector[Place] = Vector(
    Place(Rules("Larkmeadow", "A wide green bowl with ore close by. A gentle place to learn the year.", 96, 11L, Goal.Survive(1), nests = 1,
      threat = 0.6f, water = 0.42f, cliffs = 0.35f, seasons = Vector(170f, 130f, 130f, 110f), firstWave = 150f, waveSeconds = 55f, tutorial = true, wild = false, aims = Vector(to(Slay(40))(Copper -> 100), to(Build(Brazier, 2))(Coal -> 60), to(Learn(Smelter))(Tin -> 60), to(Reach(1, Season.Winter))(Coal -> 80)),
      start = Vector(Stack(Copper, 260), Stack(Tin, 60), Stack(Flint, 60), Stack(Coal, 40))), 14f, 70f, Vector.empty, Vector.empty),
    Place(Rules("Copperbrook", "A river runs through it, and the best seams lie under the water. Wait for summer.", 112, 29L, Goal.Survive(2), nests = 2,
      threat = 0.85f, water = 0.62f, cliffs = 0.45f,
      aims = Vector(to(Deliver(Bronze, 60))(Copper -> 150), to(Learn(Windmill))(Bronze -> 40), to(Reach(1, Season.Summer))(Flint -> 100), to(Build(Gearworks, 1))(Gear -> 20), to(Wave(10))(Bronze -> 100), to(Build(Boltcaster, 3))(Gear -> 40), to(Reach(2, Season.Winter))(Coal -> 200))), 28f, 54f, Vector(0), Vector.empty),
    Place(Rules("Stillwater", "A lake keeps the Bramble out, until it freezes. Strike before the second winter.", 120, 47L, Goal.Cleanse, nests = 2,
      threat = 0.95f, water = 0.85f, cliffs = 0.35f, seasons = Vector(150f, 130f, 130f, 140f), startSeason = 0,
      aims = Vector(to(Build(Boltcaster, 2))(Gear -> 20), to(Learn(Toyworks))(Gear -> 30, Flint -> 60), to(Crew(UnitKinds.Beetle, 6))(Bronze -> 80), to(Burn(1))(Glass -> 100), to(Burn(2))())), 40f, 72f, Vector(1), Vector(Blocks.Boltcaster)),
    Place(Rules("Galehead", "Cliffs, gorges and a wind that never quite drops. Build the Great Wheel.", 128, 83L, Goal.Wheel, nests = 3,
      threat = 1.05f, water = 0.45f, cliffs = 0.8f, thickets = 0.6f, seasons = Vector(140f, 120f, 180f, 130f), start = hard,
      aims = Vector(to(Build(Windmill, 4))(Bronze -> 80), to(Build(EmberForge, 1))(Glass -> 80), to(Deliver(Ember, 20))(Gear -> 60), to(Build(Almanac, 1))(Ember -> 20), to(Build(GreatWheel, 1))(Bronze -> 200), to(Reach(2, Season.Autumn))(Gear -> 100))), 62f, 36f, Vector(2, 6), Vector(Blocks.EmberForge)),
    Place(Rules("The Long Night", "Winter lasts longer than the other three together. Build the Great Wheel anyway.", 136, 131L, Goal.Wheel, nests = 4,
      threat = 1.25f, toughness = 1.15f, water = 0.6f, cliffs = 0.6f, seasons = Vector(120f, 100f, 110f, 260f), startSeason = 1, start = hard,
      aims = Vector(to(Deliver(Coal, 300))(Bronze -> 100), to(Build(Radiator, 3))(Glass -> 100), to(Build(Aegis, 1))(Steel -> 100), to(Deliver(Ember, 80))(Gear -> 100), to(Build(GreatWheel, 1))(Ember -> 40))), 86f, 20f, Vector(3, 8), Vector(Blocks.Radiator)),
    Place(Rules("Saltpan", "Hot, flat and dry. Summer takes up half the year, and fire half the rest.", 104, 211L, Goal.Survive(2), nests = 2,
      threat = 0.75f, water = 0.15f, cliffs = 0.3f, thickets = 0.2f, ore = 1.4f, seasons = Vector(100f, 240f, 100f, 90f), startSeason = 1,
      aims = Vector(to(Deliver(Bronze, 80))(Copper -> 150), to(Learn(Bellows))(Coal -> 100), to(Build(SunLens, 2))(Glass -> 60), to(Reach(1, Season.Winter))(Coal -> 100), to(Build(WagonPost, 1))(Cloth -> 30), to(Send(200))(Gear -> 60))), 26f, 88f, Vector(0), Vector(Blocks.Smelter)),
    Place(Rules("Ironscar", "Rock, and more iron than anywhere. Three years of it.", 128, 307L, Goal.Survive(3), nests = 3,
      threat = 1f, water = 0.3f, cliffs = 0.95f, ore = 1.7f, start = hard,
      aims = Vector(to(Build(Auger, 3))(Bronze -> 100), to(Deliver(Iron, 200))(Gear -> 40), to(Build(Steelworks, 2))(Coal -> 200), to(Deliver(Steel, 200))(Gear -> 80), to(Wave(20))(Steel -> 150), to(Build(MortarRow, 2))(Powder -> 100), to(Reach(3, Season.Winter))(Ember -> 40))), 46f, 34f, Vector(1), Vector(Blocks.Auger)),
    Place(Rules("Fenmere", "More water than land. In winter it is all one plain.", 128, 401L, Goal.Cleanse, nests = 3,
      threat = 1.05f, water = 1f, cliffs = 0.2f, thickets = 0.7f, seasons = Vector(140f, 110f, 130f, 170f),
      aims = Vector(to(Build(Pump, 2))(Tin -> 100), to(Learn(Slipway))(Resin -> 80, Gear -> 30), to(Crew(UnitKinds.Skiff, 5))(Bronze -> 120), to(Burn(1))(Steel -> 80), to(Crew(UnitKinds.Cutter, 2))(Resin -> 120), to(Burn(3))())), 58f, 82f, Vector(2, 5), Vector(Blocks.Pump)),
    Place(Rules("Thornwall", "Five Briar Hearts, and every one of them has had time to grow.", 144, 503L, Goal.Cleanse, nests = 5,
      threat = 1.25f, water = 0.4f, cliffs = 0.5f, thickets = 0.9f, start = hard,
      aims = Vector(to(Build(Toyworks, 2))(Gear -> 60), to(Build(Refit, 1))(Glass -> 80), to(Crew(UnitKinds.Stag, 6))(Steel -> 80), to(Burn(1))(Ember -> 30), to(Burn(3))(Steel -> 200), to(Crew(UnitKinds.Golem, 4))(Ember -> 60), to(Burn(5))())), 78f, 58f, Vector(3, 7), Vector(Blocks.Toyworks)),
    Place(Rules("Kettle Vale", "The ground steams, and the year turns fast.", 120, 601L, Goal.Wheel, nests = 3,
      threat = 1.1f, water = 0.5f, cliffs = 0.6f, ore = 1.3f, seasons = Vector(75f, 75f, 75f, 75f), waveSeconds = 42f, start = hard,
      aims = Vector(to(Build(VentWell, 2))(Bronze -> 100), to(Build(Loom, 1))(Gear -> 60), to(Deliver(Ember, 40))(Glass -> 100), to(Build(GreatHearth, 1))(Steel -> 100), to(Build(GreatWheel, 1))(Ember -> 40))), 72f, 14f, Vector(6), Vector(Blocks.VentWell))
  )
  val all: Vector[Rules] = places.map(_.rules)
}
