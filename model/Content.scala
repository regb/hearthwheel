package hearthwheel.model

/** Quarters: 0 right, 1 down, 2 left, 3 up. The y axis goes down. */
object Dir {
  val dx: Array[Int] = Array(1, 0, -1, 0)
  val dy: Array[Int] = Array(0, 1, 0, -1)
  def opposite(d: Int): Int = (d + 2) & 3
}

// ---------------------------------------------------------------- the year

/** Each season changes the rules: what wind, water and sun give, and where one
  * can walk.
  */
enum Season(
    val label: String,
    val wind: Float,
    val stream: Float,
    val sun: Float,
    val sap: Float,
    val hint: String
) {
  case Spring extends Season("Spring", 1.0f, 1.7f, 1.0f, 2.0f, "Meltwater drives the wheels, sap runs twice as fast. The Bramble heals itself.")
  case Summer extends Season("Summer", 0.6f, 0.6f, 2.0f, 1.0f, "Shallows dry out: build and march on the riverbed. The Bramble burns twice as fiercely.")
  case Autumn extends Season("Autumn", 2.0f, 1.0f, 0.8f, 1.0f, "Gales turn the windmills. Seeds ride the wind in great numbers.")
  case Winter extends Season("Winter", 1.0f, 0.2f, 0.4f, 0.0f, "Water freezes and can be crossed. Machines without warmth run at half speed.")

  def next: Season = Season.fromOrdinal((ordinal + 1) % 4)
}

// ----------------------------------------------------------------- the land

/** Floors, from the lowest to the one drawn over the others. */
enum Floor(val water: Boolean) {
  case Deep extends Floor(true)
  case Shallow extends Floor(true)
  case Sand extends Floor(false)
  case Gravel extends Floor(false)
  case Loam extends Floor(false)
  case Meadow extends Floor(false)
}
object Floor { val all: Array[Floor] = values }

/** What stands on a floor and blocks the way. */
enum Solid { case Open, Cliff, Thicket }
object Solid { val all: Array[Solid] = values }

// ---------------------------------------------------------------- the goods

/** Goods. Compared by identity, like everything declared below. */
final class Item(val id: Int, val key: String, val label: String, val about: String, val color: Int, val burn: Float = 0f, val hardness: Int = 0)

object Items {
  val Copper = Item(0, "copper", "Copper", "Soft, warm and everywhere. Almost everything starts with it.", 0xd9814a, hardness = 1)
  val Tin = Item(1, "tin", "Tin", "Turns copper into bronze.", 0xc3cfd6, hardness = 1)
  val Coal = Item(2, "coal", "Coal", "Keeps fires going, in smelters and braziers alike.", 0x4b4754, burn = 1f, hardness = 2)
  val Flint = Item(3, "flint", "Flint", "Sharp enough to sling, pure enough to melt into glass.", 0xb9a488, hardness = 1)
  val Resin = Item(4, "resin", "Resin", "Sap of the thickets. Sticky, and it burns.", 0xf0a52e, burn = 1.4f)
  val Bronze = Item(5, "bronze", "Bronze", "The metal of machines.", 0xd9a441)
  val Glass = Item(6, "glass", "Glass", "For lenses, lanterns and kites.", 0x9fe0d8)
  val Gear = Item(7, "gear", "Gear", "Clockwork parts, cut to the hair.", 0xe8c66a)
  val Ember = Item(8, "ember", "Ember", "A fire that fits in a lantern and lasts a winter.", 0xff6a3d, burn = 4f)

  val Iron = Item(9, "iron", "Iron", "Too hard for a wheel drill. Needs an auger.", 0xa08a86, hardness = 3)
  val Steel = Item(10, "steel", "Steel", "Iron with the weakness burned out of it.", 0xb4c4d4)
  val Cloth = Item(11, "cloth", "Cloth", "Flax, spun and woven. For wings, sails and bandages.", 0xf0e2c8, burn = 0.5f)
  val Starglass = Item(12, "starglass", "Starglass", "Glass that has swallowed an ember. Nothing is harder to make.", 0xc9a8ff)
  val Powder = Item(13, "powder", "Powder", "Goes off. Keep it away from fire.", 0x8a8494, burn = 1.2f)

  val all: Vector[Item] = Vector(Copper, Tin, Coal, Flint, Resin, Bronze, Glass, Gear, Ember, Iron, Steel, Cloth, Starglass, Powder)
  /** What burning makes worse. */
  def explosive(i: Item): Float = if(i eq Powder) 1f else if(i eq Ember) 0.6f else 0f
  val Count: Int = all.size
  def apply(id: Int): Item = all(id)
}

/** What runs through pipes. */
enum Fluid(val label: String, val color: Int, val coolant: Float) {
  case Water extends Fluid("Water", 0x5ab4e8, 0.35f)
  case Oil extends Fluid("Oil", 0xd8a03a, 0f)
  case Brine extends Fluid("Brine", 0xb8f0f4, 0.9f)
}

final case class Stack(item: Item, amount: Int)

/** Amounts of every item. */
final class Stock(var capacity: Int = Int.MaxValue) {
  val amounts: Array[Int] = new Array[Int](Items.Count)
  def apply(item: Item): Int = amounts(item.id)
  def has(item: Item, n: Int = 1): Boolean = amounts(item.id) >= n
  def has(cost: Seq[Stack]): Boolean = cost.forall(s => amounts(s.item.id) >= s.amount)
  def add(item: Item, n: Int): Unit = amounts(item.id) = scala.math.min(capacity, amounts(item.id) + n)
  def remove(item: Item, n: Int): Unit = amounts(item.id) = scala.math.max(0, amounts(item.id) - n)
  def total: Int = { var s = 0; var i = 0; while(i < amounts.length) { s += amounts(i); i += 1 }; s }
  def first: Int = amounts.indexWhere(_ > 0)
}

// ------------------------------------------------------------- the buildings

enum Category(val label: String) {
  case Haul extends Category("Haulage")
  case Gather extends Category("Gathering")
  case Craft extends Category("Crafting")
  case Power extends Category("Power")
  case Guard extends Category("Defense")
  case Season extends Category("Hearth & seasons")
  case Crew extends Category("Automata")
}

enum PowerSource { case Wind, Stream, Sun, Fire }

/** A projectile, or the effect of one. */
final class Shot(
    val sprite: String,
    val damage: Float,
    val speed: Float,
    val life: Float,
    val splash: Float = 0f,
    val splashDamage: Float = 0f,
    val pierce: Int = 0,
    val burn: Float = 0f,
    val slow: Float = 0f,
    val lob: Boolean = false,
    val size: Float = 10f,
    val color: Int = 0xffe6b0,
    val knock: Float = 0f,
    val homing: Float = 0f,
    val chill: Float = 0f
)

enum TurretKind { case Shooter, Lobber, Bell, Lance, Mister, Rail, Grapnel, Swatter }

/** When a governor lets its neighbours work. */
enum Watch(val label: String) {
  case InSeason extends Watch("In the season")
  case Below extends Watch("While the Hearth has less than")
  case Above extends Watch("While the Hearth has more than")
  case Foes extends Watch("While foes are near")
  case Calm extends Watch("While no foes are near")
  case Surplus extends Watch("While power is stored")
}

/** What a block does. */
enum Role {
  case Belt(speed: Float)
  case Crossing
  case Splitter
  case Sorter(inverted: Boolean)
  case Spillway(inverted: Boolean)
  case Zipline(reach: Int)
  case Store(capacity: Int)
  case Hatch(speed: Float)
  case Drill(tier: Int, base: Float)
  case Tap(seconds: Float)
  case Pump(rate: Float)
  case Pipe
  case Cistern(capacity: Float)
  case Culvert(reach: Int)
  case Press(in: Vector[Stack], out: Fluid, rate: Float, seconds: Float)
  case Chiller(rate: Float)
  case Garden(seconds: Float)
  case Burner
  case Catapult(reach: Float)
  case Boiler(output: Float, seconds: Float)
  case VentWell(output: Float)
  case Reactor(output: Float, seconds: Float)
  case Ratchet
  case Aegis(radius: Float, strength: Float)
  case Metronome(radius: Float, speed: Float)
  case Caltrop(damage: Float)
  case Gate
  case Switch
  case Governor
  case Refit(steps: Vector[(UnitKind, UnitKind)], cost: Vector[Stack], seconds: Float)
  case WagonPost
  case Hedge
  case Crafter(in: Vector[Stack], out: Stack, seconds: Float)
  case Generator(source: PowerSource, output: Float, seconds: Float)
  case Pylon(reach: Float, links: Int)
  case Flywheel(capacity: Float)
  case Brazier(radius: Float, seconds: Float)
  case Radiator(radius: Float)
  case Wall
  case Turret(kind: TurretKind, range: Float, reload: Float, ammo: Vector[(Item, Shot)], inaccuracy: Float, air: Boolean, ground: Boolean, barrels: Int)
  case Mender(radius: Float, seconds: Float, share: Float)
  case Almanac
  case GreatWheel
  case Hearth
  case Workshop(makes: Vector[UnitKind])
  case Nest
  case Thorn(range: Float, shot: Shot, reload: Float)
  case Site
  case Loom(reach: Float)
  case Sign
  case Pod(seconds: Float)
}

final class Block(
    val key: String,
    val label: String,
    val about: String,
    val size: Int,
    val hp: Float,
    val category: Option[Category],
    val cost: Vector[Stack],
    val role: Role,
    val powerUse: Float = 0f,
    val waterUse: Float = 0f,
    val rotates: Boolean = false,
    val onWater: Boolean = false,
    val chills: Boolean = true,
    val fluid: Fluid = Fluid.Water,
    val light: Float = 0f
) {
  var id: Int = -1
  /** Seconds of work for the lantern. */
  lazy val buildSeconds: Float = {
    val weight = cost.map(s => s.amount * (if(s.item.id >= 5) 1.9f else 1f)).sum
    scala.math.max(0.18f, weight * 0.022f)
  }
  def isBelt: Boolean = role.isInstanceOf[Role.Belt]
  val isWall: Boolean = role eq Role.Wall
  val isPipe: Boolean = role eq Role.Pipe
  /** Whether fluids run through it. */
  val conducts: Boolean = role match {
    case Role.Pipe | Role.Cistern(_) | Role.Culvert(_) | Role.Pump(_) => true
    case _ => false
  }
  /** Turrets that shoot something can be cooled. */
  val coolable: Boolean = role match {
    case t: Role.Turret => t.ammo.nonEmpty && t.kind != TurretKind.Mister
    case _ => false
  }
  val warmthRadius: Float = role match {
    case Role.Hearth => if(size == 3) 11f else if(size == 4) 14f else 18f
    case _ => 0f
  }
  def isTurret: Boolean = role.isInstanceOf[Role.Turret]
  def usesPower: Boolean = powerUse > 0f
  def range: Float = role match {
    case t: Role.Turret => t.range
    case Role.Mender(radius, _, _) => radius
    case Role.Brazier(radius, _) => radius
    case Role.Radiator(radius) => radius
    case Role.Pylon(reach, _) => reach
    case Role.Zipline(reach) => reach.toFloat
    case Role.Thorn(range, _, _) => range
    case Role.Culvert(reach) => reach.toFloat
    case Role.Loom(reach) => reach
    case Role.Aegis(radius, _) => radius
    case Role.Metronome(radius, _) => radius
    case Role.Catapult(reach) => reach
    case _ => 0f
  }
}

object Blocks {
  import Items._
  private def cost(stacks: (Item, Int)*): Vector[Stack] = stacks.map(Stack(_, _)).toVector

  // Haulage
  val Belt = Block("belt", "Belt", "Carries goods one way. Drag to lay a line.", 1, 45, Some(Category.Haul), cost(Copper -> 1), Role.Belt(7f), rotates = true, chills = false)
  val SwiftBelt = Block("swift_belt", "Swift Belt", "A geared belt, nearly twice as fast.", 1, 70, Some(Category.Haul), cost(Copper -> 1, Bronze -> 1), Role.Belt(12f), rotates = true, chills = false)
  val Crossing = Block("crossing", "Crossing", "Lets two lines cross without mixing.", 1, 60, Some(Category.Haul), cost(Copper -> 3), Role.Crossing, chills = false)
  val Splitter = Block("splitter", "Splitter", "Hands goods out to every side in turn.", 1, 60, Some(Category.Haul), cost(Copper -> 3), Role.Splitter, chills = false)
  val Sorter = Block("sorter", "Sorter", "Lets the chosen item go straight on and sends the rest to the sides. Tap it to choose.", 1, 60, Some(Category.Haul), cost(Copper -> 2, Tin -> 2), Role.Sorter(false), chills = false)
  val Spillway = Block("spillway", "Spillway", "Goods go straight on until the way is full, then spill to the sides.", 1, 60, Some(Category.Haul), cost(Copper -> 4, Tin -> 2), Role.Spillway(false), chills = false)
  val Zipline = Block("zipline", "Zipline", "Carries goods over whatever is in between. Build two in line, up to four tiles apart.", 1, 60, Some(Category.Haul), cost(Copper -> 6, Tin -> 6), Role.Zipline(4), chills = false)
  val Storehouse = Block("storehouse", "Storehouse", "Holds 250 of every item.", 2, 340, Some(Category.Haul), cost(Copper -> 60, Bronze -> 20), Role.Store(250), chills = false)
  val Hatch = Block("hatch", "Hatch", "Takes the chosen item, or anything, out of a storehouse or the Hearth next to it.", 1, 70, Some(Category.Haul), cost(Bronze -> 10, Gear -> 2), Role.Hatch(9f), chills = false)
  val Pipe = Block("pipe", "Pipe", "Carries water between everything it touches.", 1, 50, Some(Category.Haul), cost(Tin -> 1), Role.Pipe, chills = false)

  val Strainer = Block("strainer", "Strainer", "A sorter the other way around: the chosen item goes to the sides.", 1, 60, Some(Category.Haul), cost(Copper -> 2, Tin -> 2), Role.Sorter(true), chills = false)
  val Weir = Block("weir", "Weir", "Goods go to the sides first, and ahead once the sides are full.", 1, 60, Some(Category.Haul), cost(Copper -> 4, Tin -> 2), Role.Spillway(true), chills = false)
  val CoveredBelt = Block("covered_belt", "Covered Belt", "A swift belt in armor. Only other belts can feed it.", 1, 220, Some(Category.Haul), cost(Copper -> 1, Steel -> 1, Glass -> 1), Role.Belt(12f), rotates = true, chills = false)
  val GreatStore = Block("great_store", "Great Store", "Holds 800 of every item.", 3, 900, Some(Category.Haul), cost(Bronze -> 80, Steel -> 60), Role.Store(800), chills = false)
  val Catapult = Block("catapult", "Catapult", "Throws whole crates to another catapult, far away. Needs power. Tap two to join them.", 3, 420, Some(Category.Haul), cost(Bronze -> 60, Steel -> 50, Gear -> 40), Role.Catapult(34f), powerUse = 1.8f)
  val Cistern = Block("cistern", "Cistern", "Holds 400 of a fluid, and passes it on to all sides.", 2, 160, Some(Category.Haul), cost(Tin -> 25, Glass -> 10), Role.Cistern(400f), chills = false)
  val Culvert = Block("culvert", "Culvert", "Carries a fluid under whatever is in between, up to four tiles. Tap two to join them.", 1, 60, Some(Category.Haul), cost(Tin -> 6, Glass -> 3), Role.Culvert(4), chills = false)
  val Burner = Block("burner", "Burner", "Burns whatever is brought to it, for good.", 1, 80, Some(Category.Haul), cost(Copper -> 10, Flint -> 5), Role.Burner, chills = false, light = 0.4f)

  // Gathering
  val Bore = Block("bore", "Great Bore", "Four tiles of drill. Digs anything, fast, with power, and twice as fast with water.", 4, 700, Some(Category.Gather), cost(Copper -> 60, Steel -> 50, Gear -> 30, Glass -> 30), Role.Drill(4, 4.4f), powerUse = 2.6f, waterUse = 5f)
  val Garden = Block("garden", "Flax Garden", "Grows flax on meadow or loam and spins it into cloth. Asleep in winter, faster with water.", 3, 120, Some(Category.Gather), cost(Copper -> 30, Glass -> 15, Bronze -> 10), Role.Garden(9f), waterUse = 2f)
  val Drill = Block("drill", "Wheel Drill", "Digs the ore it stands on. Slow, but asks for nothing.", 2, 140, Some(Category.Gather), cost(Copper -> 14), Role.Drill(2, 9.6f))
  val Auger = Block("auger", "Auger", "A powered drill, three tiles wide. Twice as fast again with water.", 3, 320, Some(Category.Gather), cost(Copper -> 35, Bronze -> 25, Gear -> 6), Role.Drill(3, 6.4f), powerUse = 0.9f, waterUse = 3f)
  val Tap = Block("tap", "Sap Tap", "Draws resin from the thickets it touches. Runs twice as fast in spring and not at all in winter.", 1, 60, Some(Category.Gather), cost(Copper -> 10, Tin -> 8), Role.Tap(4.4f))
  val Pump = Block("pump", "Pump", "Lifts water out of shallows and lakes. Freezes in winter unless it is warm.", 1, 80, Some(Category.Gather), cost(Copper -> 14, Tin -> 10), Role.Pump(8f), onWater = true)

  // Crafting
  val Smelter = Block("smelter", "Smelter", "Two copper and a tin make bronze over a coal fire.", 2, 200, Some(Category.Craft), cost(Copper -> 30, Flint -> 20), Role.Crafter(Vector(Stack(Copper, 2), Stack(Tin, 1), Stack(Coal, 1)), Stack(Bronze, 2), 2.4f), light = 1f)
  val GlassKiln = Block("glass_kiln", "Glass Kiln", "Melts flint into glass.", 2, 200, Some(Category.Craft), cost(Copper -> 40, Flint -> 25, Bronze -> 5), Role.Crafter(Vector(Stack(Flint, 2), Stack(Coal, 1)), Stack(Glass, 1), 1.6f), light = 1f)
  val Gearworks = Block("gearworks", "Gearworks", "Cuts bronze into gears. Needs power.", 3, 340, Some(Category.Craft), cost(Copper -> 60, Bronze -> 35), Role.Crafter(Vector(Stack(Bronze, 2)), Stack(Gear, 1), 1.8f), powerUse = 1.1f)
  val EmberForge = Block("ember_forge", "Ember Forge", "Folds resin and coal into glass. Needs power and water.", 3, 360, Some(Category.Craft), cost(Bronze -> 60, Glass -> 40, Gear -> 20), Role.Crafter(Vector(Stack(Resin, 2), Stack(Coal, 2), Stack(Glass, 1)), Stack(Ember, 1), 3f), powerUse = 2f, waterUse = 4f, light = 1f)

  val Steelworks = Block("steelworks", "Steelworks", "Iron and coal make steel. Needs power.", 3, 420, Some(Category.Craft), cost(Bronze -> 70, Flint -> 60, Gear -> 15), Role.Crafter(Vector(Stack(Iron, 2), Stack(Coal, 2)), Stack(Steel, 1), 2f), powerUse = 1.4f, light = 1f)
  val PowderMill = Block("powder_mill", "Powder Mill", "Coal and flint, ground fine. Needs a little power.", 2, 180, Some(Category.Craft), cost(Copper -> 40, Bronze -> 20, Gear -> 5), Role.Crafter(Vector(Stack(Coal, 1), Stack(Flint, 2)), Stack(Powder, 2), 1.8f), powerUse = 0.5f)
  val StarFurnace = Block("star_furnace", "Star Furnace", "Glass, steel and an ember become starglass. Needs much power, and oil.", 3, 520, Some(Category.Craft), cost(Bronze -> 100, Steel -> 70, Glass -> 60, Ember -> 15), Role.Crafter(Vector(Stack(Glass, 2), Stack(Steel, 1), Stack(Ember, 1)), Stack(Starglass, 1), 3.6f), powerUse = 4f, waterUse = 4f, fluid = Fluid.Oil, light = 1f)
  val Press = Block("press", "Oil Press", "Presses resin into oil. Needs power.", 2, 180, Some(Category.Craft), cost(Copper -> 40, Bronze -> 25), Role.Press(Vector(Stack(Resin, 1)), Fluid.Oil, 9f, 1.2f), powerUse = 0.7f)
  val Chiller = Block("chiller", "Chiller", "Makes brine out of water, to cool turrets and reactors. Costs no power in winter.", 2, 180, Some(Category.Craft), cost(Tin -> 40, Glass -> 25, Steel -> 10), Role.Chiller(8f), powerUse = 1.2f, waterUse = 8f, chills = false)

  // Power
  val Windmill = Block("windmill", "Windmill", "Turns with the wind, twice as hard in autumn.", 2, 150, Some(Category.Power), cost(Copper -> 35, Bronze -> 6), Role.Generator(PowerSource.Wind, 0.75f, 0f), chills = false)
  val Waterwheel = Block("waterwheel", "Waterwheel", "Must touch water. Roars in spring, stands still in the ice.", 2, 180, Some(Category.Power), cost(Copper -> 40, Tin -> 20, Bronze -> 8), Role.Generator(PowerSource.Stream, 1.3f, 0f), chills = false)
  val SunLens = Block("sun_lens", "Sun Lens", "Glass that boils water on a sunny day. At its best in summer.", 2, 120, Some(Category.Power), cost(Copper -> 30, Glass -> 25, Bronze -> 10), Role.Generator(PowerSource.Sun, 1.1f, 0f), chills = false)
  val Firebox = Block("firebox", "Firebox", "Burns coal, resin or embers, in any season.", 2, 200, Some(Category.Power), cost(Copper -> 45, Bronze -> 20), Role.Generator(PowerSource.Fire, 2.4f, 4f), light = 1f, chills = false)
  val Pylon = Block("pylon", "Pylon", "Links power within six tiles. New ones connect by themselves; tap two to link or unlink them.", 1, 60, Some(Category.Power), cost(Copper -> 3, Tin -> 2), Role.Pylon(6.5f, 10), chills = false)
  val TallPylon = Block("tall_pylon", "Tall Pylon", "Links power within fifteen tiles.", 2, 160, Some(Category.Power), cost(Copper -> 20, Bronze -> 15, Glass -> 5), Role.Pylon(15f, 15), chills = false)
  val Flywheel = Block("flywheel", "Flywheel", "Stores power while there is too much of it.", 2, 180, Some(Category.Power), cost(Bronze -> 30, Gear -> 8), Role.Flywheel(120f), chills = false)

  val Boiler = Block("boiler", "Boiler", "Burns fuel under water for a great deal of power.", 3, 380, Some(Category.Power), cost(Copper -> 80, Bronze -> 50, Steel -> 25), Role.Boiler(7.5f, 2.6f), waterUse = 5f, light = 0.8f, chills = false)
  val VentWell = Block("vent_well", "Vent Well", "Stands on a steaming vent and gives power in any season.", 2, 240, Some(Category.Power), cost(Copper -> 60, Bronze -> 30, Glass -> 20), Role.VentWell(1.1f), chills = false)
  val SunCourt = Block("sun_court", "Sun Court", "Nine lenses on one stand.", 3, 280, Some(Category.Power), cost(Copper -> 60, Glass -> 90, Steel -> 20), Role.Generator(PowerSource.Sun, 3.4f, 0f), chills = false)
  val Reactor = Block("reactor", "Ember Reactor", "Embers in a ring. Tremendous power, as long as brine keeps it cool. It does not fail quietly.", 4, 900, Some(Category.Power), cost(Steel -> 150, Glass -> 100, Gear -> 80, Starglass -> 25), Role.Reactor(26f, 6f), waterUse = 6f, fluid = Fluid.Brine, light = 1.2f, chills = false)
  val Ratchet = Block("ratchet", "Ratchet", "Lets stored power through one way, from the grid behind to the one ahead, until both are as full.", 1, 80, Some(Category.Power), cost(Bronze -> 8, Gear -> 4), Role.Ratchet, rotates = true, chills = false)
  val GreatFlywheel = Block("great_flywheel", "Great Flywheel", "Stores far more.", 3, 520, Some(Category.Power), cost(Steel -> 60, Gear -> 40, Bronze -> 40), Role.Flywheel(1200f), chills = false)

  // Defense
  private val pebble = Shot("shot_pebble", 9f, 300f, 0.95f, color = 0xe9a06b, size = 9f)
  private val shard = Shot("shot_shard", 14f, 320f, 0.95f, color = 0xd8c7a8, size = 9f)
  private val slug = Shot("shot_slug", 20f, 340f, 1f, color = 0xf2c864, size = 10f, knock = 20f)
  val Slinger = Block("slinger", "Slinger", "A quick little turret for anything in reach. Feed it copper, flint or bronze.", 1, 130, Some(Category.Guard), cost(Copper -> 32), Role.Turret(TurretKind.Shooter, 8.2f, 0.34f, Vector(Copper -> pebble, Flint -> shard, Bronze -> slug), 0.06f, true, true, 2))
  val Emberpot = Block("emberpot", "Firepot", "Lobs burning pots at what walks. Coal burns, resin slows, embers do both.", 2, 300, Some(Category.Guard), cost(Copper -> 55, Bronze -> 25),
    Role.Turret(TurretKind.Lobber, 13f, 1.2f, Vector(
      Coal -> Shot("shot_pot", 8f, 190f, 2.2f, splash = 36f, splashDamage = 48f, burn = 4f, lob = true, size = 16f, color = 0xff8a3c),
      Resin -> Shot("shot_pot", 6f, 190f, 2.2f, splash = 42f, splashDamage = 34f, burn = 1.5f, slow = 3f, lob = true, size = 16f, color = 0xf0b040),
      Ember -> Shot("shot_pot", 10f, 200f, 2.2f, splash = 48f, splashDamage = 96f, burn = 6f, lob = true, size = 18f, color = 0xff5a2a)), 0.08f, false, true, 1), light = 0.6f)
  val Boltcaster = Block("boltcaster", "Boltcaster", "Throws long bolts that go through a whole line of them.", 2, 320, Some(Category.Guard), cost(Copper -> 60, Bronze -> 45, Gear -> 12),
    Role.Turret(TurretKind.Shooter, 14f, 0.85f, Vector(
      Bronze -> Shot("shot_bolt", 48f, 520f, 0.9f, pierce = 3, size = 22f, color = 0xf2c864),
      Gear -> Shot("shot_bolt", 82f, 560f, 0.9f, pierce = 6, size = 24f, color = 0xfff0b8, knock = 40f)), 0.02f, true, true, 1))
  val Bell = Block("storm_bell", "Storm Bell", "Rings a spark that leaps from foe to foe. Runs on power alone.", 2, 280, Some(Category.Guard), cost(Copper -> 50, Glass -> 30, Bronze -> 25), Role.Turret(TurretKind.Bell, 9f, 0.75f, Vector.empty, 0f, true, true, 1), powerUse = 2.2f, light = 0.5f)
  val Lance = Block("sun_lance", "Sunlance", "A burning beam. Its strength follows the sun.", 3, 560, Some(Category.Guard), cost(Bronze -> 80, Glass -> 80, Gear -> 40, Ember -> 10), Role.Turret(TurretKind.Lance, 15f, 0f, Vector.empty, 0f, true, true, 1), powerUse = 4.5f, light = 0.8f)
  val Mister = Block("mister", "Mister", "Sprays water: it slows, puts fires out, and freezes foes solid in winter.", 1, 130, Some(Category.Guard), cost(Copper -> 25, Tin -> 20), Role.Turret(TurretKind.Mister, 7f, 0.08f, Vector.empty, 0.15f, true, true, 1), waterUse = 2.5f)
  private val squib = Shot("shot_squib", 4f, 330f, 1f, splash = 34f, splashDamage = 16f, size = 10f, color = 0xffb0e0)
  private val squibFine = Shot("shot_squib", 6f, 350f, 1f, splash = 44f, splashDamage = 30f, size = 11f, color = 0xffd27a)
  val Squib = Block("squib", "Squib", "Fireworks for whatever flies. Feed it flint or powder.", 1, 140, Some(Category.Guard), cost(Copper -> 40, Tin -> 20), Role.Turret(TurretKind.Shooter, 11f, 0.42f, Vector(Flint -> squib, Powder -> squibFine), 0.12f, true, false, 1))
  private val puff = Shot("shot_flame", 9f, 190f, 0.8f, pierce = 4, burn = 3f, size = 16f, color = 0xff8a3c)
  private val puffHot = Shot("shot_flame", 17f, 210f, 0.8f, pierce = 6, burn = 6f, size = 18f, color = 0xff5a2a)
  val Bellows = Block("bellows", "Bellows", "Breathes fire over a short distance. Coal or embers.", 2, 360, Some(Category.Guard), cost(Copper -> 70, Bronze -> 30, Gear -> 5), Role.Turret(TurretKind.Shooter, 5.2f, 0.09f, Vector(Coal -> puff, Ember -> puffHot), 0.2f, true, true, 1), light = 0.6f)
  private val rocket = Shot("shot_rocket", 12f, 260f, 2.2f, splash = 34f, splashDamage = 36f, size = 15f, color = 0xffb05a, homing = 3.4f)
  private val rocketStar = Shot("shot_rocket", 24f, 300f, 2.2f, splash = 46f, splashDamage = 78f, size = 17f, color = 0xc9a8ff, homing = 4.2f)
  val Skyrack = Block("skyrack", "Skyrack", "Rockets that follow what they are sent after. Powder or starglass.", 3, 560, Some(Category.Guard), cost(Copper -> 90, Steel -> 60, Gear -> 30, Glass -> 30), Role.Turret(TurretKind.Shooter, 17f, 0.28f, Vector(Powder -> rocket, Starglass -> rocketStar), 0.35f, true, true, 4))
  private val rail = Shot("shot_bolt", 260f, 0f, 0.3f, pierce = 99, size = 10f, color = 0xf0f4ff)
  private val railStar = Shot("shot_bolt", 480f, 0f, 0.3f, pierce = 99, burn = 3f, size = 12f, color = 0xc9a8ff)
  val Longbow = Block("longbow", "Longbow", "One steel bolt through everything in line, a third of the valley away.", 3, 640, Some(Category.Guard), cost(Bronze -> 80, Steel -> 90, Gear -> 60, Starglass -> 10), Role.Turret(TurretKind.Rail, 23f, 2.6f, Vector(Steel -> rail, Starglass -> railStar), 0f, true, true, 1), powerUse = 2f)
  val MortarRow = Block("mortar_row", "Mortar Row", "Four firepots on one carriage. Powder, embers, or resin to slow them.", 3, 620, Some(Category.Guard), cost(Copper -> 100, Bronze -> 60, Steel -> 50, Gear -> 20),
    Role.Turret(TurretKind.Lobber, 18f, 0.5f, Vector(
      Resin -> Shot("shot_pot", 6f, 190f, 2.4f, splash = 40f, splashDamage = 26f, burn = 1.5f, slow = 3f, lob = true, size = 16f, color = 0xf0b040),
      Powder -> Shot("shot_pot", 8f, 190f, 2.4f, splash = 46f, splashDamage = 52f, lob = true, size = 16f, color = 0xffd27a, knock = 30f),
      Ember -> Shot("shot_pot", 12f, 200f, 2.4f, splash = 50f, splashDamage = 88f, burn = 6f, lob = true, size = 18f, color = 0xff5a2a)), 0.14f, false, true, 4), light = 0.4f)
  val Grapnel = Block("grapnel", "Grapnel", "Hooks one flyer at a time and reels it in. Needs power.", 2, 300, Some(Category.Guard), cost(Copper -> 60, Cloth -> 20, Gear -> 15), Role.Turret(TurretKind.Grapnel, 12f, 0f, Vector.empty, 0f, true, false, 1), powerUse = 1.6f)
  val Swatter = Block("swatter", "Swatter", "Swats shots out of the air. Needs power.", 2, 300, Some(Category.Guard), cost(Glass -> 50, Bronze -> 40, Steel -> 20), Role.Turret(TurretKind.Swatter, 7f, 0.22f, Vector.empty, 0f, true, true, 1), powerUse = 1.8f)
  val Caltrop = Block("caltrop", "Caltrops", "Walked over, it cuts. Wears out.", 1, 90, Some(Category.Guard), cost(Copper -> 8, Flint -> 6), Role.Caltrop(16f), chills = false)
  val Gate = Block("gate", "Gate", "A wall that your own walk through.", 1, 420, Some(Category.Guard), cost(Copper -> 8, Bronze -> 4), Role.Gate, chills = false)
  val GateLarge = Block("gate_large", "Large Gate", "Wide enough for a golem.", 2, 1700, Some(Category.Guard), cost(Copper -> 30, Bronze -> 16), Role.Gate, chills = false)
  val SteelWall = Block("steel_wall", "Steel Wall", "Hard.", 1, 1250, Some(Category.Guard), cost(Steel -> 6), Role.Wall, chills = false)
  val SteelWallLarge = Block("steel_wall_large", "Large Steel Wall", "Hard, four times.", 2, 5000, Some(Category.Guard), cost(Steel -> 24), Role.Wall, chills = false)
  val StarWall = Block("star_wall", "Starglass Wall", "Shots sometimes fly back from it.", 1, 1700, Some(Category.Guard), cost(Starglass -> 4, Steel -> 4), Role.Wall, chills = false, light = 0.2f)
  val Aegis = Block("aegis", "Aegis", "A dome that stops shots, until it has taken too many. Needs power.", 3, 600, Some(Category.Guard), cost(Steel -> 70, Glass -> 90, Gear -> 30), Role.Aegis(6.2f, 700f), powerUse = 3.2f, light = 0.5f)
  val Metronome = Block("metronome", "Metronome", "Everything around it works half as fast again. Needs power.", 2, 320, Some(Category.Guard), cost(Bronze -> 60, Gear -> 40, Glass -> 30, Starglass -> 5), Role.Metronome(7f, 1.5f), powerUse = 3f)
  val TinkerHall = Block("tinker_hall", "Tinker Hall", "Mends a wide circle, and more at a time.", 2, 360, Some(Category.Guard), cost(Bronze -> 40, Steel -> 25, Cloth -> 20), Role.Mender(12f, 2.6f, 0.12f), powerUse = 1.2f, light = 0.4f)
  val Wall = Block("wall", "Copper Wall", "Something to bite on.", 1, 340, Some(Category.Guard), cost(Copper -> 6), Role.Wall, chills = false)
  val WallLarge = Block("wall_large", "Large Copper Wall", "Four walls in one.", 2, 1360, Some(Category.Guard), cost(Copper -> 24), Role.Wall, chills = false)
  val BronzeWall = Block("bronze_wall", "Bronze Wall", "Takes far more biting.", 1, 780, Some(Category.Guard), cost(Bronze -> 6), Role.Wall, chills = false)
  val BronzeWallLarge = Block("bronze_wall_large", "Large Bronze Wall", "Four bronze walls in one.", 2, 3120, Some(Category.Guard), cost(Bronze -> 24), Role.Wall, chills = false)
  val Tinker = Block("tinker", "Tinker Post", "Mends everything around it, a little at a time.", 1, 130, Some(Category.Guard), cost(Copper -> 25, Tin -> 15, Bronze -> 5), Role.Mender(7f, 3f, 0.08f), powerUse = 0.35f, light = 0.4f)

  // Hearth and seasons
  val Brazier = Block("brazier", "Brazier", "Burns coal, resin or embers in winter to keep machines around it warm.", 1, 90, Some(Category.Season), cost(Copper -> 12, Tin -> 6), Role.Brazier(6.5f, 9f), chills = false, light = 1f)
  val Radiator = Block("radiator", "Radiator", "Warms a wide circle with power and water.", 2, 200, Some(Category.Season), cost(Bronze -> 30, Glass -> 15, Gear -> 5), Role.Radiator(11f), powerUse = 1.2f, waterUse = 1.5f, chills = false, light = 0.6f)
  val Almanac = Block("almanac", "Almanac", "Bends the year: hold a season back, or hurry it on.", 3, 420, Some(Category.Season), cost(Bronze -> 80, Glass -> 40, Gear -> 40), Role.Almanac, powerUse = 1.5f, light = 0.5f)
  val GreatWheel = Block("great_wheel", "Great Wheel", "Four spokes, each filled in a season of its own. Complete it to set the year right.", 5, 2400, Some(Category.Season), cost(Bronze -> 250, Glass -> 120, Gear -> 120), Role.GreatWheel, chills = false, light = 1f)

  val Switch = Block("switch", "Switch", "Stops and starts what it touches. Tap it.", 1, 60, Some(Category.Season), cost(Copper -> 5, Tin -> 3), Role.Switch, chills = false)
  val Governor = Block("governor", "Governor", "Lets what it touches work only at times: in a season, while goods are short, while foes are near. Tap it to choose.", 1, 80, Some(Category.Season), cost(Bronze -> 10, Gear -> 5), Role.Governor, chills = false)
  val WagonPost = Block("wagon_post", "Wagon Post", "Sends what it is given to your store outside, to begin other valleys with.", 3, 420, Some(Category.Season), cost(Copper -> 120, Bronze -> 60, Cloth -> 20), Role.WagonPost, chills = false)
  val GreatHearth = Block("great_hearth", "Great Hearth", "Built over the Hearth. Holds more, warms further, and lights a stronger lantern.", 4, 7500, Some(Category.Season), cost(Copper -> 1800, Bronze -> 500, Glass -> 200, Gear -> 100), Role.Hearth, chills = false, light = 2f)
  val GrandHearth = Block("grand_hearth", "Grand Hearth", "The greatest fire a valley can hold.", 5, 14000, Some(Category.Season), cost(Copper -> 4000, Steel -> 800, Glass -> 600, Starglass -> 120), Role.Hearth, chills = false, light = 2.6f)

  // Automata
  val Toyworks = Block("toyworks", "Toyworks", "Winds up beetles, kites, and wrens that dig by themselves. Tap it to choose which.", 3, 420, Some(Category.Crew), cost(Copper -> 80, Bronze -> 50, Gear -> 15), Role.Workshop(Vector(UnitKinds.Beetle, UnitKinds.Kite, UnitKinds.Wren)), powerUse = 1.6f)
  val Refit = Block("refit", "Refit Yard", "Makes a first automaton into a second. Send one over.", 3, 520, Some(Category.Crew), cost(Bronze -> 90, Gear -> 40, Glass -> 30), Role.Refit(Vector(UnitKinds.Beetle -> UnitKinds.Stag, UnitKinds.Kite -> UnitKinds.Lanternfly, UnitKinds.Wren -> UnitKinds.Mason), cost(Gear -> 15, Bronze -> 25, Glass -> 10), 16f), powerUse = 2.6f)
  val Foundry = Block("foundry", "Foundry", "Makes a second automaton into a third.", 4, 900, Some(Category.Crew), cost(Bronze -> 160, Steel -> 90, Gear -> 70, Ember -> 20), Role.Refit(Vector(UnitKinds.Stag -> UnitKinds.Golem, UnitKinds.Lanternfly -> UnitKinds.Heron, UnitKinds.Mason -> UnitKinds.Tender), cost(Steel -> 40, Gear -> 30, Ember -> 8, Cloth -> 15), 32f), powerUse = 4f, light = 0.6f)
  val StarForge = Block("star_forge", "Star Forge", "Makes a third automaton into the last of its line.", 5, 1600, Some(Category.Crew), cost(Steel -> 300, Gear -> 150, Starglass -> 80, Ember -> 60), Role.Refit(Vector(UnitKinds.Golem -> UnitKinds.Colossus, UnitKinds.Heron -> UnitKinds.Skybarge), cost(Steel -> 150, Starglass -> 50, Ember -> 40, Cloth -> 60), 70f), powerUse = 8f, light = 1f)

  // Not built by hand
  val Hearth = Block("hearth", "Hearth", "The heart of the valley. Do not let it go out.", 3, 3600, None, Vector.empty, Role.Hearth, chills = false, light = 1.6f)
  val Nest = Block("nest", "Briar Heart", "The Bramble grows from here. Burn it out, and it stops coming this way.", 3, 2600, None, Vector.empty, Role.Nest, chills = false)
  val Thorn = Block("thorn", "Thorn Spire", "Spits at whatever comes near the Briar Heart.", 2, 500, None, Vector.empty, Role.Thorn(9f, Shot("shot_thorn", 14f, 210f, 1.4f, size = 12f, color = 0xb4e06a), 1.1f), chills = false)
  val Hedge = Block("hedge", "Hedge", "It grows back.", 1, 420, None, Vector.empty, Role.Hedge, chills = false)
  val Pod = Block("pod", "Brood Pod", "Hatches what the season brings, and keeps the roots around it alive.", 2, 900, None, Vector.empty, Role.Pod(24f), chills = false)

  // On the water
  val Slipway = Block("slipway", "Slipway", "Launches skiffs. Must stand on water, and what it launches waits for the thaw like everyone else.", 3, 460, Some(Category.Crew), cost(Copper -> 120, Bronze -> 60, Resin -> 40, Gear -> 10), Role.Workshop(Vector(UnitKinds.Skiff)), powerUse = 1.4f, onWater = true)
  val Drydock = Block("drydock", "Drydock", "Makes a skiff into a cutter, and a cutter into an icebreaker. Send one alongside.", 4, 800, Some(Category.Crew), cost(Bronze -> 160, Steel -> 60, Gear -> 40, Resin -> 80), Role.Refit(Vector(UnitKinds.Skiff -> UnitKinds.Cutter, UnitKinds.Cutter -> UnitKinds.Icebreaker), cost(Steel -> 25, Gear -> 15, Resin -> 30), 24f), powerUse = 3f, onWater = true)

  // Thinking machines
  val Loom = Block("loom", "Card Loom", "Reads up to eight punched cards: if this, then that. Tap what it should act on to link it, then tap the loom again for its cards.", 2, 200, Some(Category.Season), cost(Bronze -> 60, Gear -> 30, Cloth -> 10), Role.Loom(12f), powerUse = 0.4f)
  val Sign = Block("sign", "Tally Board", "Shows one of the four drums that looms write to. Tap it to choose which.", 1, 60, Some(Category.Season), cost(Bronze -> 5, Glass -> 2), Role.Sign, chills = false)

  val Site = Block("site", "Building Site", "Under construction.", 1, 10, None, Vector.empty, Role.Site, chills = false)

  val all: Vector[Block] = {
    val list = Vector(
      Belt, SwiftBelt, CoveredBelt, Crossing, Splitter, Sorter, Strainer, Spillway, Weir, Zipline, Catapult, Storehouse, GreatStore, Hatch, Pipe, Culvert, Cistern, Burner,
      Drill, Auger, Bore, Tap, Garden, Pump,
      Smelter, GlassKiln, Gearworks, PowderMill, Steelworks, EmberForge, StarFurnace, Press, Chiller,
      Windmill, Waterwheel, SunLens, SunCourt, Firebox, Boiler, VentWell, Reactor, Pylon, TallPylon, Flywheel, GreatFlywheel, Ratchet,
      Slinger, Squib, Emberpot, Bellows, Boltcaster, Bell, MortarRow, Skyrack, Longbow, Lance, Mister, Grapnel, Swatter,
      Wall, WallLarge, BronzeWall, BronzeWallLarge, SteelWall, SteelWallLarge, StarWall, Gate, GateLarge, Caltrop, Tinker, TinkerHall, Aegis, Metronome,
      Brazier, Radiator, Switch, Governor, Almanac, WagonPost, GreatHearth, GrandHearth, GreatWheel,
      Toyworks, Refit, Foundry, StarForge,
      Hearth, Nest, Thorn, Hedge, Site,
      Slipway, Drydock, Loom, Sign, Pod
    )
    list.zipWithIndex.foreach { case (b, i) => b.id = i }
    list
  }
  /** Building sites, by size. */
  val sites: Array[Block] = Array.tabulate(6) { n =>
    val b = new Block("site", "Building Site", "Under construction.", scala.math.max(1, n), 60f * n * n, None, Vector.empty, Role.Site, chills = false)
    b.id = Site.id
    b
  }
  val byKey: Map[String, Block] = all.map(b => b.key -> b).toMap
  def buildable(c: Category): Vector[Block] = all.filter(_.category.contains(c))

  /** What each spoke of the Great Wheel asks for, by season. */
  val spokes: Vector[Vector[Stack]] = Vector(
    cost(Resin -> 120, Bronze -> 120),
    cost(Glass -> 150, Bronze -> 120),
    cost(Gear -> 120, Bronze -> 120),
    cost(Ember -> 60, Gear -> 60)
  )
}

// ------------------------------------------------------------- the creatures

enum Walk { case Ground, Climb, Fly, Float }

/** How a unit fights. */
enum Arms {
  case Shoots(shot: Shot, range: Float, reload: Float)
  case Bites(damage: Float, reload: Float)
  case Bursts(damage: Float, radius: Float)
  case Drops(shot: Shot, reload: Float)
  case Nothing
}

/** What a unit does besides fighting. */
enum Trade {
  case None
  /** Digs soft ores and brings them to the Hearth. */
  case Digs(perItem: Float)
  /** Builds along, and puts ruins up again. */
  case Builds
  /** Mends buildings and its own kind around it. */
  case Mends(radius: Float, perSecond: Float)
}

final class UnitKind(
    val key: String,
    val label: String,
    val about: String,
    val hp: Float,
    val speed: Float,
    val radius: Float,
    val walk: Walk,
    val arms: Arms,
    val friendly: Boolean,
    val cost: Vector[Stack] = Vector.empty,
    val seconds: Float = 0f,
    val armor: Float = 0f,
    val siege: Float = 1f,
    val boss: Boolean = false,
    val spawns: Option[String] = None,
    val legs: Int = 0,
    val tier: Int = 1,
    val trade: Trade = Trade.None,
    /** Takes hits before health does, and comes back. */
    val shield: Float = 0f
) {
  var id: Int = -1
  def flies: Boolean = walk == Walk.Fly
  def floats: Boolean = walk == Walk.Float
  /** Goes through ice, if slowly. */
  def breaksIce: Boolean = key == "icebreaker"
}

object UnitKinds {
  import Items._

  // Automata
  private val spark = Shot("shot_spark", 11f, 340f, 0.75f, size = 9f, color = 0xffd27a)
  val Lantern = UnitKind("lantern", "Lamplighter", "You.", 350, 165f, 13f, Walk.Fly, Arms.Shoots(spark, 8f, 0.2f), true)
  val LanternGreat = UnitKind("lantern_great", "Great Lamplighter", "You, brighter.", 620, 190f, 14f, Walk.Fly, Arms.Shoots(Shot("shot_spark", 18f, 360f, 0.8f, size = 10f, color = 0xffd27a), 9f, 0.17f), true, armor = 2f)
  val LanternGrand = UnitKind("lantern_grand", "Grand Lamplighter", "You, at your brightest.", 1100, 215f, 15f, Walk.Fly, Arms.Shoots(Shot("shot_spark", 26f, 400f, 0.85f, pierce = 1, size = 12f, color = 0xffe6a0), 10f, 0.15f), true, armor = 4f)
  val Beetle = UnitKind("beetle", "Beetle", "A clockwork beetle with a sling.", 190, 46f, 11f, Walk.Ground, Arms.Shoots(Shot("shot_shard", 13f, 260f, 0.7f, size = 9f, color = 0xf0c890), 5.6f, 0.5f), true, Vector(Stack(Gear, 6), Stack(Flint, 12)), 14f, armor = 2f, legs = 6)
  val Stag = UnitKind("stag", "Stag Beetle", "Larger, and with horns that throw bolts.", 520, 42f, 14f, Walk.Ground, Arms.Shoots(Shot("shot_bolt", 34f, 380f, 0.7f, pierce = 1, size = 18f, color = 0xf2c864), 7f, 0.7f), true, armor = 5f, legs = 6, tier = 2)
  val Golem = UnitKind("golem", "Golem", "A walking furnace.", 1400, 31f, 17f, Walk.Ground, Arms.Shoots(Shot("shot_pot", 22f, 230f, 0.9f, splash = 28f, splashDamage = 40f, burn = 3f, size = 14f, color = 0xff8a3c), 7f, 0.75f), true, armor = 9f, legs = 2, tier = 3)
  val Colossus = UnitKind("colossus", "Colossus", "It takes a valley's worth of starglass, and gives one back.", 6500, 26f, 28f, Walk.Climb, Arms.Shoots(Shot("shot_rocket", 60f, 320f, 1.4f, splash = 44f, splashDamage = 90f, burn = 3f, size = 18f, color = 0xc9a8ff, homing = 2f), 11f, 0.5f), true, armor = 14f, legs = 4, tier = 4, shield = 900f)
  val Kite = UnitKind("kite", "Kite", "Light, quick and hard to swat.", 100, 120f, 11f, Walk.Fly, Arms.Shoots(Shot("shot_spark", 9f, 320f, 0.55f, size = 8f, color = 0xbff0ff), 5f, 0.25f), true, Vector(Stack(Glass, 8), Stack(Gear, 4)), 12f)
  val Lanternfly = UnitKind("lanternfly", "Lanternfly", "Carries a fire from one end of the valley to the other.", 420, 92f, 15f, Walk.Fly, Arms.Shoots(Shot("shot_spark", 24f, 380f, 0.8f, pierce = 1, burn = 2f, size = 12f, color = 0xffc04a), 7.5f, 0.35f), true, armor = 3f, tier = 2)
  val Heron = UnitKind("heron", "Heron", "Drops firepots from where nothing reaches it.", 1100, 78f, 19f, Walk.Fly, Arms.Drops(Shot("shot_pot", 0f, 0f, 0.5f, splash = 46f, splashDamage = 70f, burn = 4f, size = 14f, color = 0xff8a3c), 0.5f), true, armor = 7f, tier = 3)
  val Skybarge = UnitKind("skybarge", "Skybarge", "A fortress on silk and hot air.", 5200, 42f, 34f, Walk.Fly, Arms.Shoots(Shot("shot_rocket", 40f, 340f, 1.6f, splash = 40f, splashDamage = 64f, size = 16f, color = 0xffb05a, homing = 3f), 12f, 0.28f), true, armor = 12f, tier = 4, shield = 700f)
  val Wren = UnitKind("wren", "Wren", "Digs soft ores out and carries them home.", 110, 96f, 10f, Walk.Fly, Arms.Nothing, true, Vector(Stack(Gear, 4), Stack(Resin, 10)), 12f, trade = Trade.Digs(1.5f))
  val Mason = UnitKind("mason", "Mason", "Builds along with you, and puts ruins up again by itself.", 340, 88f, 13f, Walk.Fly, Arms.Shoots(Shot("shot_spark", 10f, 320f, 0.6f, size = 8f, color = 0xa8ffc0), 5f, 0.5f), true, armor = 2f, tier = 2, trade = Trade.Builds)
  val Tender = UnitKind("tender", "Tender", "Mends buildings and automata around it.", 900, 70f, 17f, Walk.Fly, Arms.Shoots(Shot("shot_spark", 16f, 320f, 0.7f, size = 9f, color = 0xa8ffc0), 6f, 0.4f), true, armor = 5f, tier = 3, trade = Trade.Mends(5f, 22f))

  val Skiff = UnitKind("skiff", "Skiff", "Quick under sail, as long as there is water.", 240, 74f, 12f, Walk.Float, Arms.Shoots(Shot("shot_bolt", 18f, 360f, 0.8f, size = 14f, color = 0xf2c864), 8f, 0.6f), true, Vector(Stack(Resin, 10), Stack(Gear, 6), Stack(Copper, 20)), 16f, armor = 2f)
  val Cutter = UnitKind("cutter", "Cutter", "Throws firepots far inland.", 720, 58f, 16f, Walk.Float, Arms.Shoots(Shot("shot_pot", 8f, 240f, 3f, splash = 40f, splashDamage = 52f, burn = 3f, lob = true, size = 14f, color = 0xff8a3c), 13f, 1.2f), true, armor = 5f, tier = 2)
  val Icebreaker = UnitKind("icebreaker", "Icebreaker", "Sails all year, through whatever is in the way.", 2600, 44f, 24f, Walk.Float, Arms.Shoots(Shot("shot_rocket", 40f, 330f, 1.6f, splash = 42f, splashDamage = 80f, size = 17f, color = 0xbfeaff, homing = 1.5f), 15f, 0.7f), true, armor = 10f, tier = 3, shield = 250f)

  // The Bramble
  val Thistledown = UnitKind("thistledown", "Thistledown", "Looks for somewhere to take root. Shoot it down before it lands.", 260, 40f, 13f, Walk.Fly, Arms.Nothing, false, tier = 2)
  val Burr = UnitKind("burr", "Burr", "Rolls at the nearest wall and bursts.", 62, 58f, 9f, Walk.Ground, Arms.Bursts(26f, 28f), false)
  val Nettle = UnitKind("nettle", "Nettle", "Keeps its distance and stings.", 125, 42f, 10f, Walk.Ground, Arms.Shoots(Shot("shot_sting", 11f, 200f, 1f, size = 9f, color = 0xc6f06a), 5.2f, 0.9f), false, legs = 4)
  val Frostling = UnitKind("frostling", "Frostling", "Only walks in winter, and in numbers. What it hits stiffens.", 150, 62f, 10f, Walk.Ground, Arms.Shoots(Shot("shot_rime", 9f, 220f, 0.9f, size = 9f, color = 0xbfeaff, slow = 1.5f, chill = 1.2f), 4.4f, 0.7f), false, legs = 4)
  val Catkin = UnitKind("catkin", "Catkin", "Lobs spores further than a slinger reaches.", 240, 30f, 12f, Walk.Ground, Arms.Shoots(Shot("shot_pod", 6f, 150f, 2.6f, splash = 34f, splashDamage = 30f, lob = true, size = 12f, color = 0xe8d06a), 11.5f, 2.1f), false, armor = 1f, legs = 4, tier = 2)
  val Stumper = UnitKind("stumper", "Stumper", "A stump with a grudge against buildings.", 620, 27f, 16f, Walk.Ground, Arms.Bites(46f, 1f), false, armor = 4f, siege = 1.6f, legs = 4, tier = 2)
  val Dandel = UnitKind("dandel", "Dandel", "Drifts over the walls and drops seeds.", 85, 62f, 10f, Walk.Fly, Arms.Drops(Shot("shot_seed", 0f, 0f, 0.5f, splash = 26f, splashDamage = 20f, size = 9f, color = 0xf4f0d8), 0.8f), false)
  val Dewmoth = UnitKind("dewmoth", "Dewmoth", "Heals the others. Get it first.", 220, 56f, 12f, Walk.Fly, Arms.Nothing, false, tier = 2, trade = Trade.Mends(5f, 14f))
  val Hornwing = UnitKind("hornwing", "Hornwing", "Hunts turrets.", 260, 88f, 12f, Walk.Fly, Arms.Shoots(Shot("shot_sting", 15f, 260f, 0.8f, size = 10f, color = 0xffc46a), 5f, 0.5f), false, armor = 2f, tier = 2)
  val Creepvine = UnitKind("creepvine", "Creepvine", "Climbs cliffs and thickets alike.", 440, 36f, 14f, Walk.Climb, Arms.Shoots(Shot("shot_thorn", 24f, 220f, 1.2f, size = 12f, color = 0xb4e06a), 6.5f, 1.1f), false, armor = 3f, legs = 6, tier = 2)
  val Podder = UnitKind("podder", "Podder", "Full of burrs.", 900, 24f, 18f, Walk.Ground, Arms.Shoots(Shot("shot_pod", 10f, 150f, 2f, splash = 30f, splashDamage = 30f, size = 14f, color = 0xd8a0d0), 8f, 1.8f), false, armor = 5f, spawns = Some("burr"), legs = 4, tier = 3)
  val Stormseed = UnitKind("stormseed", "Stormseed", "A cloud of seeds around a core of spite.", 950, 48f, 20f, Walk.Fly, Arms.Drops(Shot("shot_seed", 0f, 0f, 0.5f, splash = 46f, splashDamage = 55f, size = 14f, color = 0xd8c8f0), 0.55f), false, armor = 5f, tier = 3)
  val Briarhulk = UnitKind("briarhulk", "Briarhulk", "A hedge that has learned to walk.", 2600, 24f, 24f, Walk.Ground, Arms.Bites(120f, 1.1f), false, armor = 9f, siege = 1.8f, legs = 4, tier = 4)
  val Gnarl = UnitKind("gnarl", "Old Gnarl", "Older than the valley.", 4800, 20f, 30f, Walk.Climb, Arms.Shoots(Shot("shot_thorn", 60f, 240f, 1.6f, splash = 34f, splashDamage = 40f, size = 20f, color = 0xa8e05a), 7.4f, 0.8f), false, armor = 3f, siege = 1.5f, boss = true, spawns = Some("nettle"), legs = 6, tier = 5)
  val Queen = UnitKind("queen", "The Seed Queen", "Autumn's own. She does not land.", 5600, 34f, 30f, Walk.Fly, Arms.Shoots(Shot("shot_sting", 30f, 280f, 1.4f, size = 14f, color = 0xffc46a), 8f, 0.22f), false, armor = 4f, boss = true, spawns = Some("dandel"), tier = 5)
  val Rimewood = UnitKind("rimewood", "The Rimewood", "Winter, walking.", 15000, 17f, 38f, Walk.Climb, Arms.Shoots(Shot("shot_rime", 80f, 260f, 1.7f, splash = 46f, splashDamage = 70f, size = 24f, slow = 2f, color = 0xbfeaff), 11f, 0.6f), false, armor = 8f, siege = 1.6f, boss = true, spawns = Some("frostling"), legs = 6, tier = 5)

  /** The lamplighters, by the size of the Hearth that lights them. */
  var lanterns: Vector[UnitKind] = Vector.empty
  def isLantern(k: UnitKind): Boolean = (k eq Lantern) || (k eq LanternGreat) || (k eq LanternGrand)

  val all: Vector[UnitKind] = {
    val list = Vector(
      Lantern, LanternGreat, LanternGrand, Beetle, Stag, Golem, Colossus, Kite, Lanternfly, Heron, Skybarge, Wren, Mason, Tender,
      Burr, Nettle, Frostling, Catkin, Stumper, Dandel, Dewmoth, Hornwing, Creepvine, Podder, Stormseed, Briarhulk, Gnarl, Queen, Rimewood,
      Skiff, Cutter, Icebreaker, Thistledown
    )
    lanterns = Vector(Lantern, LanternGreat, LanternGrand)
    list.zipWithIndex.foreach { case (k, i) => k.id = i }
    list
  }
  val byKey: Map[String, UnitKind] = all.map(k => k.key -> k).toMap
}

// ------------------------------------------------------------------ learning

/** A page of the almanac: pay its cost from the Hearth to learn the block. */
final class Page(val block: Block, val requires: Vector[Block], val cost: Vector[Stack], val column: Int, val row: Float)

object Pages {
  import Blocks._
  import Items._
  private def p(b: Block, column: Int, row: Float, requires: Block*)(stacks: (Item, Int)*): Page =
    Page(b, requires.toVector, stacks.map(Stack(_, _)).toVector, column, row)

  /** Known from the start. */
  val basics: Set[Block] = Set(Belt, Drill, Slinger, Wall)

  val all: Vector[Page] = Vector(
    p(Belt, 0, 1f)(), p(Drill, 0, 5f)(), p(Slinger, 0, 10f)(), p(Wall, 0, 13f)(),
    // Haulage
    p(Splitter, 1, 0f, Belt)(Copper -> 40), p(Crossing, 1, 1f, Belt)(Copper -> 40), p(Zipline, 1, 2f, Belt)(Copper -> 60, Tin -> 30),
    p(Sorter, 2, 0f, Splitter)(Copper -> 60, Tin -> 40), p(Strainer, 3, 0f, Sorter)(Copper -> 60, Tin -> 40), p(Spillway, 4, 0f, Strainer)(Copper -> 80, Tin -> 60), p(Weir, 5, 0f, Spillway)(Copper -> 80, Tin -> 60),
    p(SwiftBelt, 2, 1f, Crossing)(Copper -> 120, Bronze -> 60), p(Storehouse, 3, 1f, SwiftBelt)(Copper -> 200, Bronze -> 80), p(Hatch, 4, 1f, Storehouse)(Bronze -> 100, Gear -> 20), p(CoveredBelt, 5, 1f, Hatch, Steelworks)(Steel -> 60, Glass -> 60), p(GreatStore, 6, 1f, CoveredBelt)(Steel -> 150), p(Catapult, 7, 1f, GreatStore)(Steel -> 200, Gear -> 120),
    p(Burner, 2, 2f, Zipline)(Copper -> 80),
    // Gathering and crafting
    p(Tap, 1, 3f, Drill)(Copper -> 80, Tin -> 60), p(Pump, 2, 3f, Tap)(Copper -> 100, Tin -> 80), p(Pipe, 3, 3f, Pump)(Tin -> 60), p(Culvert, 4, 3f, Pipe)(Tin -> 80, Glass -> 30), p(Cistern, 5, 3f, Culvert)(Tin -> 120, Glass -> 60),
    p(Press, 6, 3f, Cistern, Gearworks)(Bronze -> 120, Resin -> 60), p(Chiller, 7, 3f, Press, Steelworks)(Steel -> 80, Glass -> 100),
    p(Garden, 4, 4f, GlassKiln, Pipe)(Glass -> 60, Bronze -> 60), p(Auger, 5, 4f, Gearworks)(Bronze -> 200, Gear -> 50), p(Bore, 6, 4f, Auger, Steelworks)(Steel -> 160, Gear -> 80),
    p(Smelter, 1, 5f, Drill)(Copper -> 120, Flint -> 60), p(GlassKiln, 2, 5f, Smelter)(Copper -> 120, Bronze -> 40), p(Gearworks, 3, 5f, GlassKiln, Windmill)(Bronze -> 160, Copper -> 200), p(PowderMill, 4, 5f, Gearworks)(Bronze -> 80, Gear -> 10), p(Steelworks, 5, 5f, Auger)(Bronze -> 220, Gear -> 60, Iron -> 60), p(EmberForge, 6, 5f, Gearworks, Pump)(Bronze -> 250, Glass -> 150, Gear -> 80), p(StarFurnace, 7, 5f, EmberForge, Steelworks, Press)(Steel -> 200, Ember -> 40),
    // Power
    p(Windmill, 2, 6f, Smelter)(Copper -> 100, Bronze -> 40), p(Pylon, 3, 6f, Windmill)(Copper -> 60, Tin -> 40), p(TallPylon, 4, 6f, Pylon)(Bronze -> 100, Glass -> 40), p(Flywheel, 5, 6f, TallPylon, Gearworks)(Bronze -> 140, Gear -> 40), p(GreatFlywheel, 6, 6f, Flywheel, Steelworks)(Steel -> 150, Gear -> 80), p(Ratchet, 7, 6f, GreatFlywheel)(Bronze -> 60, Gear -> 30),
    p(Waterwheel, 2, 7f, Smelter)(Copper -> 140, Bronze -> 50), p(Firebox, 3, 7f, Waterwheel)(Copper -> 180, Bronze -> 90), p(SunLens, 4, 7f, Firebox)(Glass -> 80, Bronze -> 60), p(VentWell, 5, 7f, SunLens)(Bronze -> 120, Glass -> 80), p(Boiler, 6, 7f, VentWell, Steelworks)(Steel -> 120, Bronze -> 150), p(SunCourt, 7, 7f, Boiler)(Glass -> 200, Steel -> 80), p(Reactor, 8, 7f, SunCourt, StarFurnace, Chiller)(Steel -> 400, Starglass -> 60),
    // Warmth and seasons
    p(Brazier, 1, 8f, Drill)(Copper -> 60, Coal -> 60), p(Switch, 2, 8f, Brazier)(Copper -> 60, Tin -> 40), p(Governor, 4, 8f, Switch, Gearworks)(Bronze -> 80, Gear -> 30), p(Radiator, 5, 8f, Brazier, Gearworks, Pipe)(Bronze -> 150, Glass -> 100), p(WagonPost, 6, 8f, Radiator, Garden)(Bronze -> 200, Cloth -> 40), p(Almanac, 7, 8f, Gearworks, SunLens)(Glass -> 150, Gear -> 80), p(GreatWheel, 8, 8f, Almanac, EmberForge)(Bronze -> 400, Glass -> 250, Gear -> 150),
    p(GreatHearth, 5, 9f, Gearworks)(Copper -> 800, Bronze -> 300), p(GrandHearth, 8, 9f, GreatHearth, StarFurnace)(Steel -> 600, Starglass -> 60),
    // Defense
    p(Emberpot, 1, 10f, Slinger)(Copper -> 150, Bronze -> 60), p(Boltcaster, 3, 10f, Emberpot, Gearworks)(Bronze -> 180, Gear -> 40), p(MortarRow, 5, 10f, Boltcaster, Steelworks, PowderMill)(Steel -> 120, Powder -> 100), p(Longbow, 8, 10f, MortarRow, StarFurnace)(Steel -> 250, Gear -> 150, Starglass -> 40),
    p(Squib, 1, 11f, Slinger)(Copper -> 120, Tin -> 60), p(Bellows, 2, 11f, Emberpot)(Copper -> 200, Bronze -> 80), p(Bell, 4, 11f, Emberpot, Pylon)(Glass -> 100, Bronze -> 100), p(Skyrack, 6, 11f, Bell, MortarRow)(Steel -> 160, Powder -> 150), p(Lance, 7, 11f, Bell, EmberForge)(Glass -> 250, Gear -> 120, Ember -> 40),
    p(Tinker, 1, 12f, Slinger)(Copper -> 120, Tin -> 80, Bronze -> 30), p(Mister, 4, 12f, Pipe)(Copper -> 150, Tin -> 100), p(Grapnel, 5, 12f, Squib, Garden)(Cloth -> 60, Gear -> 40), p(TinkerHall, 6, 12f, Tinker, Steelworks, Garden)(Steel -> 100, Cloth -> 60), p(Swatter, 7, 12f, Bell, Steelworks)(Glass -> 150, Steel -> 80), p(Aegis, 8, 12f, Swatter)(Steel -> 250, Glass -> 250, Gear -> 80), p(Metronome, 8, 11f, StarFurnace, TinkerHall)(Gear -> 150, Starglass -> 30),
    p(WallLarge, 1, 13f, Wall)(Copper -> 120), p(BronzeWall, 2, 13f, WallLarge)(Bronze -> 100), p(BronzeWallLarge, 3, 13f, BronzeWall)(Bronze -> 200), p(SteelWall, 5, 13f, BronzeWallLarge, Steelworks)(Steel -> 150), p(SteelWallLarge, 6, 13f, SteelWall)(Steel -> 300), p(StarWall, 8, 13f, SteelWallLarge, StarFurnace)(Starglass -> 50),
    p(Caltrop, 1, 14f, Wall)(Copper -> 80, Flint -> 60), p(Gate, 2, 14f, WallLarge)(Copper -> 100, Bronze -> 40), p(GateLarge, 3, 14f, Gate)(Bronze -> 120),
    // Automata
    p(Toyworks, 5, 14f, Gearworks, Boltcaster)(Bronze -> 200, Gear -> 60), p(Refit, 6, 14f, Toyworks)(Bronze -> 250, Gear -> 100, Glass -> 80), p(Foundry, 7, 14f, Refit, EmberForge, Steelworks)(Steel -> 300, Gear -> 150, Ember -> 40), p(StarForge, 8, 14f, Foundry, StarFurnace)(Steel -> 600, Starglass -> 120),
    p(Slipway, 6, 9f, Gearworks, Boltcaster)(Bronze -> 160, Resin -> 80, Gear -> 30), p(Drydock, 7, 9f, Slipway, Steelworks)(Steel -> 120, Gear -> 80, Resin -> 120),
    p(Loom, 6, 10f, Governor, Garden)(Bronze -> 160, Gear -> 60, Cloth -> 20), p(Sign, 7, 10f, Loom)(Glass -> 40, Bronze -> 60)
  )
  val byBlock: Map[Block, Page] = all.map(p => p.block -> p).toMap
}
