package hearthwheel.model

import scala.collection.mutable
import MathUtil._

/** Things worth showing that leave no trace in the rules. */
enum FxKind { case Puff, Hit, Muzzle, Explosion, Flame, Mist, Mend, Build, Break, Death, Spark, Ice, Splash, Beam, Leaf, Ring }
enum SoundKind { case Sling, Lob, Bolt, Zap, Beam, Spray, Boom, Sting, Spark, Chomp, Built, Place, Break, Season, Wave, Alarm, Learn, Click, Deny, Win, Lose, Ice, Death, Rail, Rocket, Flame, Dig, Deed, Horn }

final class Fx(val kind: FxKind, val x: Float, val y: Float, val a: Float, val color: Int, val x2: Float = 0f, val y2: Float = 0f)
final class Heard(val kind: SoundKind, val x: Float, val y: Float)

enum Outcome { case Playing, Won, Lost }

/** Something to tell the player. */
final case class Notice(text: String, color: Int)

/** A building that was destroyed, and can be put up again. */
final case class Ruin(block: Block, x: Int, y: Int, rotation: Int, config: Int, note: String = "")

/** A valley and everything going on in it. Knows nothing of how it is shown. */
final class Valley(val width: Int, val height: Int, val rules: Rules) {
  import Valley._

  val floors = new Array[Byte](width * height)
  val ores = Array.fill[Byte](width * height)(-1)
  val solids = new Array[Byte](width * height)
  /** A little variety for each tile, for the view. */
  val grain = new Array[Byte](width * height)
  private val builtOn = new Array[Building](width * height)

  val rng = new Rng(rules.seed * 7919L + 17L)
  val buildings = mutable.ArrayBuffer.empty[Building]
  val creatures = mutable.ArrayBuffer.empty[Creature]
  val shots = new Shots(this)
  val networks = new Networks(this)
  val waves = new Waves(this)
  val builder = new Builder(this)
  val ruins = mutable.ArrayBuffer.empty[Ruin]
  /** Goods sent out by wagon, to be counted in the store outside. */
  val sentOut = new Array[Int](Items.Count)
  val fires = new Fires(this)
  val sky = new Sky(this)
  val ledger = new Ledger(this)
  val roots = new Roots(this)
  /** Numbers that looms keep, for all of them. */
  val drums = new Array[Int](4)
  def drum(i: Int, value: Int): Unit = drums(clamp(i, 0, drums.length - 1)) = clamp(value, -9999, 9999)
  /** What has been met, for the codex. */
  val met = mutable.LinkedHashSet.empty[UnitKind]
  val learned = mutable.LinkedHashSet.empty[Block]

  var hearth: HearthBuilding = null
  var lantern: Creature = null
  var lanternRespawn = 0f
  var time = 0f
  var loading = false
  var outcome = Outcome.Playing

  // For the deeds.
  var iceTook = false
  var nestsBurned = 0
  val felled = mutable.HashSet.empty[String]

  // Numbers for the end screen and the tutorial.
  var built = 0
  var lost = 0
  var slain = 0
  val gatheredCounts = new Array[Int](Items.Count)
  val deliveredCounts = new Array[Int](Items.Count)
  /** Items per minute into the Hearth, smoothed. */
  val rates = new Array[Float](Items.Count)
  private val recent = new Array[Float](Items.Count)
  private var rateClock = 0f

  // What the view takes and clears.
  val effects = mutable.ArrayBuffer.empty[Fx]
  val sounds = mutable.ArrayBuffer.empty[Heard]
  val notices = mutable.ArrayBuffer.empty[Notice]
  /** Bumped whenever the land or what stands on it changes. */
  var revision = 0

  private var nextId = 1
  def freshId(): Int = { nextId += 1; nextId }

  // ------------------------------------------------------------- the year

  var seasonIndex = rules.startSeason
  var seasonTime = 0f
  var year = 1
  /** Seconds during which the season does not advance. */
  var held = 0f
  /** Set once the Great Wheel is complete: the last winter never ends by itself. */
  var longWinter = false

  def season: Season = Season.fromOrdinal(seasonIndex)
  def seasonLength: Float = rules.seasons(seasonIndex)
  def seasonProgress: Float = if(longWinter) 0.5f else clamp(seasonTime / seasonLength, 0f, 1f)
  def secondsLeft: Float = scala.math.max(0f, seasonLength - seasonTime)
  /** Whether shallows are wet and lakes are open. */
  def waterOpen: Boolean = season == Season.Spring || season == Season.Autumn
  def frozen: Boolean = season == Season.Winter
  def dry: Boolean = season == Season.Summer

  def holdSeason(seconds: Float): Unit = {
    held += seconds
    notices += Notice(s"${season.label} lingers", 0xe8c66a)
  }
  def hastenSeason(): Unit = if(!longWinter) advanceSeason()

  private def advanceSeason(): Unit = {
    seasonTime = 0f
    held = 0f
    seasonIndex = (seasonIndex + 1) % 4
    if(seasonIndex == rules.startSeason) year += 1
    sounds += new Heard(SoundKind.Season, -1f, -1f)
    notices += Notice(s"${season.label} has come", 0xffe9b0)
    landChanged()
    if(season == Season.Spring || season == Season.Summer) thaw()
    recheckGoal()
  }

  // The ice goes, and what was standing on it goes with it.
  private def thaw(): Unit = creatures.toVector.foreach { c =>
    if(!c.kind.flies && inside(c.tileX, c.tileY) && floorAt(c.tileX, c.tileY) == Floor.Deep) {
      fx(FxKind.Splash, c.x, c.y, c.kind.radius, 0)
      if(c.team == Bramble) iceTook = true
      if(c.kind.boss) c.hurt(c.maxHp * 0.4f, pierceArmor = true)
      else killCreature(c)
      sound(SoundKind.Ice, c.x, c.y)
    }
  }

  // ------------------------------------------------------------- the land

  def inside(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
  def tileIndex(x: Int, y: Int): Int = clamp(y, 0, height - 1) * width + clamp(x, 0, width - 1)
  def floorAt(x: Int, y: Int): Floor = if(inside(x, y)) Floor.all(floors(y * width + x)) else Floor.Gravel
  def windNow: Float = season.wind * sky.wind
  def sunNow: Float = season.sun * sky.sun
  def flyingNow: Float = sky.flying
  /** The seam on a tile by its index, or -1. */
  def oreTile(i: Int): Int = { val o = ores(i); if(o < 0 || o >= Items.Count) -1 else o.toInt }
  def ventAt(x: Int, y: Int): Boolean = inside(x, y) && ores(y * width + x) == Vent
  def ventsUnder(block: Block, x: Int, y: Int): Int = {
    var n = 0
    for(ty <- y until y + block.size; tx <- x until x + block.size) if(ventAt(tx, ty)) n += 1
    n
  }
  def solidAt(x: Int, y: Int): Solid = if(inside(x, y)) Solid.all(solids(y * width + x)) else Solid.Cliff
  def oreAt(x: Int, y: Int): Int = if(inside(x, y)) oreTile(y * width + x) else -1
  def buildingAt(x: Int, y: Int): Building = if(inside(x, y)) builtOn(y * width + x) else null

  /** Whether there is water there now, if under ice. */
  def afloat(tile: Int): Boolean = floors(tile) == Floor.Deep.ordinal || (floors(tile) == Floor.Shallow.ordinal && !dry)

  /** Whether a creature cannot stand on that tile. */
  def blocks(c: Creature, x: Int, y: Int): Boolean = {
    if(!inside(x, y)) return true
    val i = y * width + x
    if(c.kind.walk == Walk.Climb) {
      val b = builtOn(i)
      return b != null && b.team != c.team
    }
    if(c.kind.walk == Walk.Float) return !afloat(i) || builtOn(i) != null
    if(solids(i) != 0) return true
    if(floors(i) == Floor.Deep.ordinal && !frozen) return true
    val b = builtOn(i)
    b != null && !walkable(b, c.team)
  }

  /** Whether those of a team walk over a building as if it was not there. */
  private def walkable(b: Building, team: Int): Boolean =
    b.isInstanceOf[CaltropBuilding] || (b.team == team && (b.isInstanceOf[BeltBuilding] || (b.block.role eq Role.Gate)))

  /** What it costs to walk onto a tile, or -1 when it cannot be. */
  def walkCost(tile: Int, walk: Walk, team: Int): Int = {
    val b = builtOn(tile)
    if(walk == Walk.Float) return if(afloat(tile) && b == null) 2 else -1
    if(walk == Walk.Climb) {
      val base = if(solids(tile) != 0) 2 else if(floors(tile) == Floor.Deep.ordinal && !frozen) 3 else 1
      return if(b != null && b.team != team) base + 24 else base
    }
    if(solids(tile) != 0) return -1
    val floor = floors(tile)
    if(floor == Floor.Deep.ordinal && !frozen) return -1
    var cost = if(floor == Floor.Shallow.ordinal && waterOpen) 4 else 2
    if(b != null && !walkable(b, team)) {
      if(b.team == team) return -1
      else if(team == Player) return -1
      else cost += (if(b.block.isWall) 62 else 40)
    }
    // Keep a little off the cliffs.
    val x = tile % width
    if(x > 0 && solids(tile - 1) != 0) cost += 1
    if(x < width - 1 && solids(tile + 1) != 0) cost += 1
    if(tile >= width && solids(tile - width) != 0) cost += 1
    if(tile + width < solids.length && solids(tile + width) != 0) cost += 1
    cost
  }

  val groundField = new FlowField(this, Walk.Ground, forPlayer = false)
  val climbField = new FlowField(this, Walk.Climb, forPlayer = false)
  private val orderFields = mutable.LinkedHashMap.empty[(Int, Walk), FlowField]
  def fieldTo(tile: Int, walk: Walk): FlowField = orderFields.getOrElseUpdate((tile, walk), {
    if(orderFields.size >= 12) orderFields.remove(orderFields.head._1)
    val f = new FlowField(this, walk, forPlayer = true)
    f.setGoals(Array(tile))
    f.finish()
    f
  })
  private var fieldWait = 0f
  private var landDirty = true
  private def landChanged(): Unit = { landDirty = true; revision += 1 }

  private def updateFields(dt: Float): Unit = {
    fieldWait -= dt
    if(landDirty && fieldWait <= 0f) {
      landDirty = false
      fieldWait = 0.5f
      groundField.invalidate()
      climbField.invalidate()
      orderFields.valuesIterator.foreach(_.invalidate())
    }
    groundField.update(6000)
    climbField.update(6000)
    orderFields.valuesIterator.foreach(_.update(3000))
  }

  // ------------------------------------------------------------- warmth

  private val warmth = new Array[Byte](width * height)
  private var warmthDirty = true
  def warmthChanged(): Unit = warmthDirty = true
  def isWarm(x: Int, y: Int): Boolean = inside(x, y) && warmth(y * width + x) > 0
  private def rebuildWarmth(): Unit = {
    warmthDirty = false
    java.util.Arrays.fill(warmth, 0.toByte)
    buildings.foreach {
      case h: HearthBuilding => warmCircle(h, h.block.warmthRadius * sky.warmth)
      case b: BrazierBuilding => if(b.lit) warmCircle(b, b.radius * sky.warmth)
      case r: RadiatorBuilding => if(r.lit) warmCircle(r, r.radius * sky.warmth)
      case _ => ()
    }
    buildings.foreach { b => b.warm = isWarm(floor(b.cx / Tile), floor(b.cy / Tile)) }
    revision += 1
  }
  private def warmCircle(b: Building, radius: Float): Unit = {
    val bx = b.cx / Tile
    val by = b.cy / Tile
    var y = scala.math.max(0, floor(by - radius))
    while(y <= scala.math.min(height - 1, floor(by + radius))) {
      var x = scala.math.max(0, floor(bx - radius))
      while(x <= scala.math.min(width - 1, floor(bx + radius))) {
        val dx = x + 0.5f - bx
        val dy = y + 0.5f - by
        if(dx * dx + dy * dy <= radius * radius) warmth(y * width + x) = 1
        x += 1
      }
      y += 1
    }
  }

  // ------------------------------------------------------------ the Hearth

  def hearthStock(item: Item): Int = if(hearth == null) 0 else hearth.stock(item)
  def canPay(cost: Seq[Stack]): Boolean = rules.free || (hearth != null && hearth.stock.has(cost))
  def pay(cost: Seq[Stack]): Unit = if(!rules.free && hearth != null) cost.foreach(s => hearth.stock.remove(s.item, s.amount))
  def pay1(item: Item): Unit = if(hearth != null) hearth.stock.remove(item, 1)
  def refund(item: Item, n: Int): Unit = if(hearth != null && !rules.free) hearth.stock.add(item, n)
  def gathered(item: Item, n: Int): Unit = gatheredCounts(item.id) += n
  def crafted(item: Item, n: Int): Unit = gatheredCounts(item.id) += n
  def delivered(item: Item, n: Int): Unit = { deliveredCounts(item.id) += n; recent(item.id) += n }

  def knows(b: Block): Boolean = rules.free || learned.contains(b)
  def canLearn(p: Page): Boolean = !knows(p.block) && p.requires.forall(knows) && canPay(p.cost)
  def learn(p: Page): Boolean = if(canLearn(p)) {
    pay(p.cost)
    learned += p.block
    notices += Notice(s"Learned: ${p.block.label}", 0xa8e0c0)
    sound(SoundKind.Learn)
    true
  } else false

  def spokeDone(wheel: GreatWheelBuilding, spoke: Int): Unit = {
    val name = Season.fromOrdinal(spoke).label
    notices += Notice(s"The $name spoke is complete", 0xffd27a)
    sound(SoundKind.Win)
    fx(FxKind.Ring, wheel.cx, wheel.cy, 160f, 0xffd27a)
    if(wheel.complete && !longWinter) {
      longWinter = true
      seasonIndex = Season.Winter.ordinal
      seasonTime = 0f
      landChanged()
      warmthChanged()
      notices += Notice("The Long Winter begins. The Rimewood is walking.", 0xbfeaff)
      waves.summonRimewood()
    }
  }

  // ---------------------------------------------------------- effects

  def fx(kind: FxKind, x: Float, y: Float, a: Float, color: Int): Unit =
    if(effects.size < 4000) effects += new Fx(kind, x, y, a, color)
  def line(kind: FxKind, x: Float, y: Float, x2: Float, y2: Float, a: Float, color: Int): Unit =
    if(effects.size < 4000) effects += new Fx(kind, x, y, a, color, x2, y2)
  def sound(kind: SoundKind, x: Float = -1f, y: Float = -1f): Unit =
    if(sounds.size < 400) sounds += new Heard(kind, x, y)

  // -------------------------------------------------------- buildings

  def make(block: Block, x: Int, y: Int, rotation: Int, team: Int, siteTarget: Block = null): Building = block.role match {
    case Role.Belt(speed) => new BeltBuilding(this, block, x, y, rotation, team, speed)
    case Role.Crossing => new CrossingBuilding(this, block, x, y, rotation, team)
    case Role.Splitter => new SplitterBuilding(this, block, x, y, rotation, team)
    case Role.Sorter(inverted) => new SorterBuilding(this, block, x, y, rotation, team, inverted)
    case Role.Spillway(inverted) => new SpillwayBuilding(this, block, x, y, rotation, team, inverted)
    case Role.Pipe => new ConduitBuilding(this, block, x, y, rotation, team, 5f)
    case Role.Cistern(capacity) => new ConduitBuilding(this, block, x, y, rotation, team, capacity)
    case Role.Culvert(reach) => new CulvertBuilding(this, block, x, y, rotation, team, reach)
    case p: Role.Press => new PressBuilding(this, block, x, y, rotation, team, p)
    case Role.Chiller(rate) => new ChillerBuilding(this, block, x, y, rotation, team, rate)
    case Role.Garden(seconds) => new GardenBuilding(this, block, x, y, rotation, team, seconds)
    case Role.Burner => new BurnerBuilding(this, block, x, y, rotation, team)
    case Role.Catapult(reach) => new CatapultBuilding(this, block, x, y, rotation, team, reach)
    case Role.Boiler(output, seconds) => new BoilerBuilding(this, block, x, y, rotation, team, output, seconds)
    case Role.VentWell(output) => new VentBuilding(this, block, x, y, rotation, team, output)
    case Role.Reactor(output, seconds) => new ReactorBuilding(this, block, x, y, rotation, team, output, seconds)
    case Role.Ratchet => new RatchetBuilding(this, block, x, y, rotation, team)
    case Role.Aegis(radius, strength) => new AegisBuilding(this, block, x, y, rotation, team, radius, strength)
    case Role.Metronome(radius, speed) => new MetronomeBuilding(this, block, x, y, rotation, team, radius, speed)
    case Role.Caltrop(damage) => new CaltropBuilding(this, block, x, y, rotation, team, damage)
    case Role.Switch => new SwitchBuilding(this, block, x, y, rotation, team)
    case Role.Governor => new GovernorBuilding(this, block, x, y, rotation, team)
    case r: Role.Refit => new RefitBuilding(this, block, x, y, rotation, team, r)
    case Role.WagonPost => new WagonBuilding(this, block, x, y, rotation, team)
    case Role.Zipline(reach) => new ZiplineBuilding(this, block, x, y, rotation, team, reach)
    case Role.Store(capacity) => new StoreBuilding(this, block, x, y, rotation, team, capacity)
    case Role.Hatch(speed) => new HatchBuilding(this, block, x, y, rotation, team, speed)
    case Role.Drill(_, base) => new DrillBuilding(this, block, x, y, rotation, team, base)
    case Role.Tap(seconds) => new TapBuilding(this, block, x, y, rotation, team, seconds)
    case Role.Pump(rate) => new PumpBuilding(this, block, x, y, rotation, team, rate)
    case c: Role.Crafter => new CrafterBuilding(this, block, x, y, rotation, team, c)
    case g: Role.Generator => new GeneratorBuilding(this, block, x, y, rotation, team, g)
    case Role.Pylon(reach, links) => new PylonBuilding(this, block, x, y, rotation, team, reach, links)
    case Role.Flywheel(capacity) => new FlywheelBuilding(this, block, x, y, rotation, team, capacity)
    case Role.Brazier(radius, seconds) => new BrazierBuilding(this, block, x, y, rotation, team, radius, seconds)
    case Role.Radiator(radius) => new RadiatorBuilding(this, block, x, y, rotation, team, radius)
    case t: Role.Turret => new TurretBuilding(this, block, x, y, rotation, team, t)
    case Role.Mender(radius, seconds, share) => new MenderBuilding(this, block, x, y, rotation, team, radius, seconds, share)
    case Role.Almanac => new AlmanacBuilding(this, block, x, y, rotation, team)
    case Role.GreatWheel => new GreatWheelBuilding(this, block, x, y, rotation, team)
    case Role.Hearth => new HearthBuilding(this, block, x, y, rotation, team)
    case Role.Workshop(makes) => new WorkshopBuilding(this, block, x, y, rotation, team, makes)
    case Role.Nest => new NestBuilding(this, block, x, y, rotation, team)
    case t: Role.Thorn => new ThornBuilding(this, block, x, y, rotation, team, t)
    case Role.Loom(reach) => new LoomBuilding(this, block, x, y, rotation, team, reach)
    case Role.Sign => new SignBuilding(this, block, x, y, rotation, team)
    case Role.Pod(seconds) => new PodBuilding(this, block, x, y, rotation, team, seconds)
    case Role.Site => new SiteBuilding(this, block, x, y, rotation, team, siteTarget)
    case Role.Wall | Role.Gate | Role.Hedge => new PlainBuilding(this, block, x, y, rotation, team)
  }

  /** Puts a building up at once, over whatever was there. */
  def place(block: Block, x: Int, y: Int, rotation: Int, team: Int, config: Int = -1, siteTarget: Block = null): Building = {
    val b = make(block, x, y, rotation, team, siteTarget)
    add(b)
    if(config >= 0) b.configure(config)
    b.onPlaced()
    // The closest pylon with room takes what is put up within its reach.
    if(!loading && b.isPowered && !b.isInstanceOf[PylonBuilding] && b.links.isEmpty) {
      var best: PylonBuilding = null
      var i = 0
      while(i < buildings.size) {
        buildings(i) match {
          case p: PylonBuilding if p.alive && p.canLink(b) && (best == null || dist2(p.cx, p.cy, b.cx, b.cy) < dist2(best.cx, best.cy, b.cx, b.cy)) => best = p
          case _ => ()
        }
        i += 1
      }
      if(best != null) link(best, b)
    }
    b
  }

  def add(b: Building): Unit = {
    var ty = b.y
    while(ty < b.y + b.size) {
      var tx = b.x
      while(tx < b.x + b.size) {
        val old = buildingAt(tx, ty)
        if(old != null) removeBuilding(old)
        if(inside(tx, ty)) builtOn(ty * width + tx) = b
        tx += 1
      }
      ty += 1
    }
    b.id = freshId()
    buildings += b
    b match {
      case a: AegisBuilding => domes += a
      case h: HearthBuilding =>
        hearth = h
        val goals = for(gy <- h.y until h.y + h.size; gx <- h.x until h.x + h.size) yield gy * width + gx
        groundField.setGoals(goals.toArray)
        climbField.setGoals(goals.toArray)
        warmthDirty = true
      case _ => ()
    }
    b.warm = isWarm(floor(b.cx / Tile), floor(b.cy / Tile))
    around(b)
    networks.changed(b)
    landChanged()
  }

  private def around(b: Building): Unit = {
    b.neighboursChanged()
    var i = -1
    while(i <= b.size) {
      def touch(x: Int, y: Int): Unit = { val n = buildingAt(x, y); if(n != null && !(n eq b)) n.neighboursChanged() }
      touch(b.x + i, b.y - 1)
      touch(b.x + i, b.y + b.size)
      touch(b.x - 1, b.y + i)
      touch(b.x + b.size, b.y + i)
      i += 1
    }
  }
  /** Tells the neighbours that a building turned or changed. */
  def refresh(b: Building): Unit = around(b)

  def removeBuilding(b: Building): Unit = if(b.alive) {
    b.alive = false
    var ty = b.y
    while(ty < b.y + b.size) {
      var tx = b.x
      while(tx < b.x + b.size) {
        if(inside(tx, ty) && (builtOn(ty * width + tx) eq b)) builtOn(ty * width + tx) = null
        tx += 1
      }
      ty += 1
    }
    b.links.toVector.foreach(unlink(b, _))
    b.onRemoved()
    removedAny = true
    around(b)
    networks.changed(b)
    landChanged()
  }
  private var removedAny = false

  def link(a: Building, b: Building): Unit = {
    if(!a.links.contains(b)) a.links += b
    if(!b.links.contains(a)) b.links += a
    networks.powerChanged()
    revision += 1
  }
  def unlink(a: Building, b: Building): Unit = {
    a.links -= b
    b.links -= a
    networks.powerChanged()
    revision += 1
  }

  def destroyBuilding(b: Building): Unit = if(b.alive) {
    fx(FxKind.Break, b.cx, b.cy, b.size.toFloat, 0)
    sound(if(b.size > 1) SoundKind.Boom else SoundKind.Break, b.cx, b.cy)
    removeBuilding(b)
    fires.burst(b)
    if(b.team == Player) {
      lost += 1
      b match {
        case s: SiteBuilding => if(!s.removing) remember(Ruin(s.target, s.x, s.y, s.rotation, s.config, s.note))
        case h: HearthBuilding =>
          fx(FxKind.Explosion, h.cx, h.cy, 160f, 0xff8a3c)
          finish(Outcome.Lost)
        case _ => if(b.block.category.nonEmpty) remember(Ruin(b.block, b.x, b.y, b.rotation, b.config, b.note))
      }
    } else if(b.isInstanceOf[NestBuilding]) {
      nestsBurned += 1
      notices += Notice("A Briar Heart burns out", 0xffc27a)
      fx(FxKind.Explosion, b.cx, b.cy, 90f, 0xa6d86a)
      recheckGoal()
    }
  }
  private def remember(r: Ruin): Unit = {
    ruins += r
    if(ruins.size > 400) { val _ = ruins.remove(0) }
  }

  def nests: Vector[Building] = buildings.iterator.filter(b => b.alive && b.isInstanceOf[NestBuilding]).toVector

  private def finish(result: Outcome): Unit = if(outcome == Outcome.Playing) {
    outcome = result
    sound(if(result == Outcome.Won) SoundKind.Win else SoundKind.Lose)
  }

  // --------------------------------------------------------------- aims

  /** How many of the aims of the valley have been reached. */
  var aimsReached = 0
  var wagonsSent = 0
  def yearsSeen: Int = year
  def seasonsInto: Int = (seasonIndex - rules.startSeason + 4) % 4
  def aim: Option[(Aim, Vector[Stack])] = rules.aims.lift(aimsReached)
  private var aimWait = 1f
  private def pursue(dt: Float): Unit = {
    aimWait -= dt
    if(aimWait <= 0f) {
      aimWait = 0.5f
      aim.foreach { case (a, reward) =>
        if(a.done(this) >= a.goal) {
          aimsReached += 1
          reward.foreach(s => if(hearth != null) hearth.stock.add(s.item, s.amount))
          notices += Notice(s"Done: ${a.label}", 0xffd27a)
          sound(SoundKind.Learn)
        }
      }
    }
  }

  def recheckGoal(): Unit = rules.goal match {
    case Goal.Survive(years) => if(year > years) finish(Outcome.Won)
    case Goal.Cleanse => if(nests.isEmpty) finish(Outcome.Won)
    case Goal.Wheel | Goal.Endless => ()
  }

  // ------------------------------------------------------------- placing

  /** Why a block cannot go there, or null when it can. */
  def placementProblem(block: Block, x: Int, y: Int, rotation: Int): String = {
    var ty = y
    var wet = 0
    var rooted = false
    while(ty < y + block.size) {
      var tx = x
      while(tx < x + block.size) {
        if(!inside(tx, ty)) return "Out of bounds"
        if(solidAt(tx, ty) != Solid.Open) return "Something is in the way"
        val f = floorAt(tx, ty)
        rooted ||= roots.at(tx, ty)
        if(f.water) wet += 1
        if(f == Floor.Deep && !block.onWater) return "Too deep"
        if(f == Floor.Shallow && !block.onWater && !dry && !wetProof(block)) return "Only when it is dry, in summer"
        val b = buildingAt(tx, ty)
        if(b != null && !replaceable(b, block, x, y, rotation)) return "Already built on"
        tx += 1
      }
      ty += 1
    }
    if(block.onWater && wet == 0) return "Must be on water"
    if(rooted) return "Roots in the way: burn them out"
    block.role match {
      case Role.Drill(_, _) => if(DrillBuilding.survey(this, block, x, y)._2 == 0) return "No ore here"
      case Role.Tap(_) => if(TapBuilding.trees(this, x, y) == 0) return "Must touch a thicket"
      case Role.Garden(_) => if(GardenBuilding.fertility(this, block, x, y) <= 0f) return "Needs meadow or loam"
      case Role.VentWell(_) => if(ventsUnder(block, x, y) == 0) return "Must stand on a vent"
      case Role.Hearth =>
        if(hearth == null || hearth.size >= block.size) return "Must grow out of a smaller Hearth"
        if(hearth.x < x || hearth.y < y || hearth.x + hearth.size > x + block.size || hearth.y + hearth.size > y + block.size) return "Must cover the Hearth"
        if(!canPay(block.cost)) return "The Hearth lacks the goods"
      case Role.Generator(PowerSource.Stream, _, _) => if(!touchesWater(block, x, y)) return "Must touch water"
      case Role.GreatWheel => if(buildings.exists(b => b.alive && (b.isInstanceOf[GreatWheelBuilding] || isSiteOf(b, block)) && !(b.x == x && b.y == y))) return "There is only one"
      case _ => ()
    }
    val margin = NestClearance * Tile
    if(buildings.exists(b => b.team == Bramble && b.isInstanceOf[NestBuilding] && dist2(b.cx, b.cy, (x + block.size / 2f) * Tile, (y + block.size / 2f) * Tile) < margin * margin)) return "Too close to a Briar Heart"
    null
  }
  /** What can stand in running water. */
  private def wetProof(block: Block): Boolean = block.role match {
    case Role.Belt(_) | Role.Crossing | Role.Zipline(_) | Role.Wall | Role.Gate | Role.Pipe | Role.Culvert(_) | Role.Pylon(_, _) | Role.Generator(PowerSource.Stream, _, _) => true
    case _ => false
  }
  private def isSiteOf(b: Building, block: Block): Boolean = b match {
    case s: SiteBuilding => s.target eq block
    case _ => false
  }
  def touchesWater(block: Block, x: Int, y: Int): Boolean = {
    var i = 0
    while(i < block.size) {
      if(floorAt(x + i, y - 1).water || floorAt(x + i, y + block.size).water || floorAt(x - 1, y + i).water || floorAt(x + block.size, y + i).water) return true
      var j = 0
      while(j < block.size) { if(floorAt(x + i, y + j).water) return true; j += 1 }
      i += 1
    }
    false
  }

  /** Whether the new block may take the place of what is there. */
  def replaceable(old: Building, block: Block, x: Int, y: Int, rotation: Int): Boolean = {
    if(old.team != Player) return false
    if(block.role eq Role.Hearth) return old.isInstanceOf[HearthBuilding] || old.isInstanceOf[BeltBuilding]
    old match {
      case s: SiteBuilding => s.x == x && s.y == y && s.size == block.size
      case _ =>
        if(old.block eq block) block.rotates && old.rotation != rotation && old.x == x && old.y == y
        else group(old.block) != 0 && group(old.block) == group(block) && (
          (old.x == x && old.y == y && old.size == block.size) || (group(block) == 2 && block.size > old.size)
        )
    }
  }
  private def group(b: Block): Int = b.role match {
    case Role.Belt(_) | Role.Crossing | Role.Splitter | Role.Sorter(_) | Role.Spillway(_) | Role.Zipline(_) => 1
    case Role.Wall | Role.Gate => 2
    case Role.Pipe | Role.Culvert(_) => 5
    case Role.Pylon(_, _) if b.size == 1 => 3
    case Role.Drill(_, _) => 4
    case _ => 0
  }

  /** Builds a larger Hearth over the Hearth, at once: it must not go out. */
  def growHearth(block: Block, x: Int, y: Int): Unit = if(placementProblem(block, x, y, 0) == null) {
    pay(block.cost)
    val goods = hearth.stock.amounts.clone()
    val old = hearth
    hearth = null
    removeBuilding(old)
    val _ = place(block, x, y, 0, Player)
    goods.indices.foreach(i => hearth.stock.add(Items.all(i), goods(i)))
    if(lantern != null) {
      // The lantern burns brighter with it.
      val now = lantern
      now.alive = false
      lantern = spawn(UnitKinds.lanterns(hearth.tier), Player, now.x, now.y)
    }
    fx(FxKind.Ring, hearth.cx, hearth.cy, 140f, 0xffb05a)
    sound(SoundKind.Win)
    notices += Notice(s"The ${block.label} burns", 0xffd27a)
  }
  /** How many automata may be about. */
  def crewLimit: Int = if(hearth == null) 0 else 12 + hearth.tier * 12
  def crewCount: Int = creatures.count(c => c.alive && c.team == Player && !UnitKinds.isLantern(c.kind))

  /** Starts building, as the lamplighter would. */
  def startSite(block: Block, x: Int, y: Int, rotation: Int, config: Int, note: String = ""): SiteBuilding = {
    val old = buildingAt(x, y)
    old match {
      case s: SiteBuilding if (s.target eq block) && s.x == x && s.y == y && !s.removing => return s
      case _ => ()
    }
    // What is replaced goes back to the Hearth.
    val covered = mutable.HashSet.empty[Building]
    for(ty <- y until y + block.size; tx <- x until x + block.size) {
      val b = buildingAt(tx, ty)
      if(b != null) covered += b
    }
    covered.foreach {
      case s: SiteBuilding => refundSite(s)
      case b => b.block.cost.foreach(c => refund(c.item, c.amount))
    }
    val site = place(Blocks.sites(block.size), x, y, rotation, Player, siteTarget = block).asInstanceOf[SiteBuilding]
    site.config0 = config
    site.note0 = note
    site
  }
  def startRemoval(b: Building): SiteBuilding = b match {
    case s: SiteBuilding =>
      s.removing = true
      s
    case _ =>
      val site = make(Blocks.sites(b.size), b.x, b.y, b.rotation, Player, b.block).asInstanceOf[SiteBuilding]
      site.removing = true
      site.progress = 1f
      site.config0 = b.config
      site.note0 = b.note
      site.target.cost.indices.foreach(i => site.spent(i) = scala.math.ceil(site.target.cost(i).amount * 0.8).toInt)
      add(site)
      site
  }
  private def refundSite(s: SiteBuilding): Unit =
    s.target.cost.indices.foreach(i => refund(s.target.cost(i).item, s.spent(i)))

  def finishSite(s: SiteBuilding): Unit = {
    val config = s.config0
    val b = place(s.target, s.x, s.y, s.rotation, Player, config)
    if(s.note.nonEmpty) b.renote(s.note)
    built += 1
    ruins.filterInPlace(r => !(r.x < b.x + b.size && r.x + r.block.size > b.x && r.y < b.y + b.size && r.y + r.block.size > b.y))
    fx(FxKind.Build, b.cx, b.cy, b.size.toFloat, 0)
    sound(SoundKind.Built, b.cx, b.cy)
  }
  def finishRemoval(s: SiteBuilding): Unit = {
    removeBuilding(s)
    fx(FxKind.Puff, s.cx, s.cy, 0f, 0)
    sound(SoundKind.Place, s.cx, s.cy)
  }

  // ------------------------------------------------------------- creatures

  /** The closest place to one where a kind can be, up to some tiles away. */
  def standing(kind: UnitKind, x: Float, y: Float): (Float, Float) = if(kind.flies) (x, y) else {
    val tx = floor(x / Tile)
    val ty = floor(y / Tile)
    def fits(a: Int, b: Int): Boolean = inside(a, b) && walkCost(tileIndex(a, b), kind.walk, Player) >= 0
    if(fits(tx, ty)) (x, y)
    else {
      var best: (Float, Float) = null
      var bestD = Float.MaxValue
      for(dy <- -9 to 9; dx <- -9 to 9 if fits(tx + dx, ty + dy)) {
        val px = (tx + dx + 0.5f) * Tile
        val py = (ty + dy + 0.5f) * Tile
        val d = dist2(x, y, px, py)
        if(d < bestD) { bestD = d; best = (px, py) }
      }
      if(best == null) (x, y) else best
    }
  }

  def spawn(kind: UnitKind, team: Int, x: Float, y: Float): Creature = {
    val c = new Creature(this, kind, team, x, y)
    c.id = freshId()
    creatures += c
    if(!UnitKinds.isLantern(kind)) met += kind
    c
  }

  def killCreature(c: Creature): Unit = if(c.alive) {
    c.alive = false
    fx(FxKind.Death, c.x, c.y, c.kind.radius, if(c.team == Bramble) 0x8fbf5a else 0xe8c66a)
    sound(SoundKind.Death, c.x, c.y)
    if(c.team == Bramble) {
      slain += 1
      if(c.kind.boss) felled += c.kind.key
    }
    if(UnitKinds.isLantern(c.kind)) {
      lantern = null
      lanternRespawn = 4f
      notices += Notice("Your lantern went out. The Hearth lights another.", 0xffb08a)
    }
    if(c.kind eq UnitKinds.Rimewood) {
      fx(FxKind.Explosion, c.x, c.y, 220f, 0xbfeaff)
      finish(Outcome.Won)
    }
  }

  // A grid of cells, rebuilt on every step, to find creatures by place.
  private val gridWidth = width * Tile / Cell + 2
  private val gridHeight = height * Tile / Cell + 2
  private val gridHead = Array.fill(gridWidth * gridHeight)(-1)
  private var gridNext = new Array[Int](256)
  private var gridCount = 0

  private def cellOf(x: Float, y: Float): Int =
    clamp((y / Cell).toInt, 0, gridHeight - 1) * gridWidth + clamp((x / Cell).toInt, 0, gridWidth - 1)

  private def rebuildGrid(): Unit = {
    java.util.Arrays.fill(gridHead, -1)
    gridCount = creatures.size
    if(gridNext.length < gridCount) gridNext = new Array[Int](gridCount * 2)
    var i = 0
    while(i < gridCount) {
      val c = creatures(i)
      val cell = cellOf(c.x, c.y)
      gridNext(i) = gridHead(cell)
      gridHead(cell) = i
      i += 1
    }
  }

  // Calls for every creature around, some of which are a little too far.
  private inline def scan(x: Float, y: Float, r: Float)(inline f: Creature => Unit): Unit = {
    val reach = r + MaxRadius
    val x0 = clamp(((x - reach) / Cell).toInt, 0, gridWidth - 1)
    val x1 = clamp(((x + reach) / Cell).toInt, 0, gridWidth - 1)
    val y0 = clamp(((y - reach) / Cell).toInt, 0, gridHeight - 1)
    val y1 = clamp(((y + reach) / Cell).toInt, 0, gridHeight - 1)
    var gy = y0
    while(gy <= y1) {
      var gx = x0
      while(gx <= x1) {
        var i = gridHead(gy * gridWidth + gx)
        while(i >= 0) {
          if(i < creatures.size) {
            val c = creatures(i)
            if(c.alive) f(c)
          }
          i = gridNext(i)
        }
        gx += 1
      }
      gy += 1
    }
  }

  def eachCreature(x: Float, y: Float, r: Float, team: Int)(f: Creature => Unit): Unit =
    scan(x, y, r) { c =>
      val reach = r + c.kind.radius
      if(c.team == team && dist2(x, y, c.x, c.y) <= reach * reach) f(c)
    }

  def closestCreature(x: Float, y: Float, r: Float, team: Int, air: Boolean, ground: Boolean): Creature = {
    var best: Creature = null
    var bestDistance = Float.MaxValue
    scan(x, y, r) { c =>
      if(c.team == team && (if(c.kind.flies) air else ground)) {
        val d = dist2(x, y, c.x, c.y)
        val reach = r + c.kind.radius
        if(d <= reach * reach && d < bestDistance) { best = c; bestDistance = d }
      }
    }
    best
  }

  def closestWhere(x: Float, y: Float, r: Float, team: Int)(fits: Creature => Boolean): Creature = {
    var best: Creature = null
    var bestDistance = r * r
    scan(x, y, r) { c =>
      if(c.team == team && !UnitKinds.isLantern(c.kind)) {
        val d = dist2(x, y, c.x, c.y)
        if(d <= bestDistance && fits(c)) { best = c; bestDistance = d }
      }
    }
    best
  }

  /** Whether a dome of the other side takes a shot. */
  def shielded(f: Flying): Boolean = {
    var i = 0
    while(i < domes.size) {
      val a = domes(i)
      if(a.alive && a.team != f.team && a.open) {
        val r = a.reach
        if(dist2(f.x, f.y, a.cx, a.cy) < r * r) {
          a.absorb(f.shot.damage + f.shot.splashDamage, f.x, f.y)
          return true
        }
      }
      i += 1
    }
    false
  }
  private val domes = mutable.ArrayBuffer.empty[AegisBuilding]

  def ignite(b: Building, chance: Float): Unit = if(rng.chance(chance)) fires.start(b.x + rng.int(b.size), b.y + rng.int(b.size))

  /** What a digger should dig: of the soft seams close to the Hearth that are
    * open, one of the kind there is the least of.
    */
  def oreToDig(c: Creature): Int = {
    if(hearth == null) return -1
    val hx = floor(hearth.cx / Tile)
    val hy = floor(hearth.cy / Tile)
    val bestOf = Array.fill(Items.Count)(-1)
    val score = Array.fill(Items.Count)(Float.MaxValue)
    var y = scala.math.max(0, hy - 24)
    while(y <= scala.math.min(height - 1, hy + 24)) {
      var x = scala.math.max(0, hx - 24)
      while(x <= scala.math.min(width - 1, hx + 24)) {
        val i = y * width + x
        val o = oreTile(i)
        if(o >= 0 && Items.all(o).hardness <= 2 && builtOn(i) == null && solids(i) == 0 && (!Floor.all(floors(i)).water || dry)) {
          val d = distI(x, y, hx, hy) + ((i * 31 + c.id * 17) & 7)
          if(d < score(o)) { score(o) = d; bestOf(o) = i }
        }
        x += 1
      }
      y += 1
    }
    var best = -1
    var least = Int.MaxValue
    var k = 0
    while(k < Items.Count) {
      if(bestOf(k) >= 0 && hearth.stock(Items.all(k)) < least) { least = hearth.stock(Items.all(k)); best = bestOf(k) }
      k += 1
    }
    best
  }

  /** The creature a shot is in, if any that it has not gone through. */
  def hitCreature(f: Flying): Creature = {
    var found: Creature = null
    val r = f.shot.size * 0.35f
    scan(f.x, f.y, r) { c =>
      if(found == null && c.team != f.team && (if(c.kind.flies) f.air else f.ground)) {
        val reach = r + c.kind.radius
        if(dist2(f.x, f.y, c.x, c.y) <= reach * reach) {
          var seen = false
          var i = 0
          while(i < f.struckCount) { if(f.struck(i) == c.id) seen = true; i += 1 }
          if(!seen) found = c
        }
      }
    }
    found
  }

  // --------------------------------------------------- finding buildings

  def eachBuilding(x: Float, y: Float, r: Float, team: Int)(f: Building => Unit): Unit = {
    val x0 = clamp(floor((x - r) / Tile), 0, width - 1)
    val x1 = clamp(floor((x + r) / Tile), 0, width - 1)
    val y0 = clamp(floor((y - r) / Tile), 0, height - 1)
    val y1 = clamp(floor((y + r) / Tile), 0, height - 1)
    var ty = y0
    while(ty <= y1) {
      var tx = x0
      while(tx <= x1) {
        val b = builtOn(ty * width + tx)
        // Each once: at the first of its tiles within the rectangle.
        if(b != null && b.team == team && tx == scala.math.max(b.x, x0) && ty == scala.math.max(b.y, y0)) {
          val reach = r + b.size * Tile / 2f
          if(dist2(x, y, b.cx, b.cy) <= reach * reach) f(b)
        }
        tx += 1
      }
      ty += 1
    }
  }

  def closestBuilding(x: Float, y: Float, r: Float, team: Int): Building = {
    var best: Building = null
    var bestDistance = Float.MaxValue
    eachBuilding(x, y, r, team) { b =>
      val d = dist2(x, y, b.cx, b.cy) - b.size * b.size * Tile * Tile / 4f
      if(d < bestDistance) { best = b; bestDistance = d }
    }
    best
  }

  def enemyBuildingUnder(x: Float, y: Float, team: Int): Boolean = {
    val b = buildingAt(floor(x / Tile), floor(y / Tile))
    b != null && b.team != team
  }

  /** What a flyer goes for: the nearest of its favorites, or the Hearth. */
  def prey(c: Creature, turretsFirst: Boolean): Building = {
    var best: Building = null
    var bestScore = Float.MaxValue
    var i = 0
    while(i < buildings.size) {
      val b = buildings(i)
      if(b.alive && b.team != c.team && !b.isInstanceOf[BeltBuilding]) {
        val liked = if(turretsFirst) b.block.isTurret else !b.block.isWall
        val d = dist2(c.x, c.y, b.cx, b.cy) * (if(liked) 1f else 4f) * (if(b.isInstanceOf[HearthBuilding]) 2f else 1f)
        if(d < bestScore) { best = b; bestScore = d }
      }
      i += 1
    }
    best
  }

  // ------------------------------------------------------------- violence

  def explode(x: Float, y: Float, radius: Float, damage: Float, team: Int, burn: Float, slow: Float, air: Boolean, ground: Boolean, color: Int): Unit = {
    fx(FxKind.Explosion, x, y, radius, color)
    sound(SoundKind.Boom, x, y)
    eachCreature(x, y, radius, 1 - team) { c =>
      if(if(c.kind.flies) air else ground) {
        val d = dist(x, y, c.x, c.y)
        val share = clamp(1.15f - d / (radius + c.kind.radius), 0.25f, 1f)
        if(burn > 0f) c.burn = scala.math.max(c.burn, burn)
        if(slow > 0f) c.slow = scala.math.max(c.slow, slow)
        val a = atan2(c.y - y, c.x - x)
        c.push(cos(a) * 60f * share / c.mass, sin(a) * 60f * share / c.mass)
        c.hurt(damage * share)
      }
    }
    if(ground) {
      val hit = mutable.ArrayBuffer.empty[Building]
      eachBuilding(x, y, radius, 1 - team)(b => hit += b)
      hit.foreach { b =>
        b.damage(damage)
        if(burn > 0f && b.alive) ignite(b, 0.25f)
      }
      if(burn > 0f && team == Player) {
        roots.clear(x, y, radius)
        val (tx, ty) = (floor(x / Tile), floor(y / Tile))
        if(roots.at(tx, ty)) fires.start(tx, ty)
      }
    }
  }

  /** A spark that leaps on, weaker every time. */
  def chain(x: Float, y: Float, first: Creature, team: Int, damage: Float, leaps: Int, reach: Float): Unit = {
    val struck = mutable.ArrayBuffer.empty[Creature]
    var fromX = x
    var fromY = y
    var c = first
    var power = damage
    var i = 0
    while(i < leaps && c != null) {
      line(FxKind.Spark, fromX, fromY, c.x, c.y, power / damage, 0x9fe8ff)
      struck += c
      fromX = c.x
      fromY = c.y
      c.hurt(power, pierceArmor = true)
      power *= 0.82f
      var best: Creature = null
      var bestDistance = Float.MaxValue
      scan(fromX, fromY, reach) { o =>
        if(o.team != team && !struck.contains(o)) {
          val d = dist2(fromX, fromY, o.x, o.y)
          if(d < reach * reach && d < bestDistance) { best = o; bestDistance = d }
        }
      }
      c = best
      i += 1
    }
  }

  /** Damages all that is on a line. */
  def beam(x: Float, y: Float, angle: Float, length: Float, halfWidth: Float, team: Int, damage: Float, burn: Float): Unit = {
    val dx = cos(angle)
    val dy = sin(angle)
    scan(x + dx * length / 2f, y + dy * length / 2f, length / 2f + halfWidth) { c =>
      if(c.team != team) {
        val along = (c.x - x) * dx + (c.y - y) * dy
        val off = scala.math.abs(-(c.x - x) * dy + (c.y - y) * dx)
        if(along > 0f && along < length && off < halfWidth + c.kind.radius) {
          c.burn = scala.math.max(c.burn, burn)
          c.hurt(damage, pierceArmor = true)
          if(rng.chance(0.2f)) fx(FxKind.Flame, c.x, c.y, 0f, 0)
        }
      }
    }
    // Structures once each, however many of their tiles it goes over.
    var at = 0f
    var last: Building = null
    while(at < length) {
      val b = buildingAt(floor((x + dx * at) / Tile), floor((y + dy * at) / Tile))
      if(b != null && b.team != team && !(b eq last)) { b.damage(damage); last = b }
      at += Tile / 2f
    }
  }

  // ---------------------------------------------------------------- update

  /** Moves everything on by one step. */
  def update(dt: Float): Unit = {
    time += dt
    if(outcome == Outcome.Playing) {
      if(held > 0f) held = scala.math.max(0f, held - dt)
      else if(!longWinter) {
        seasonTime += dt
        if(seasonTime >= seasonLength) advanceSeason()
      }
      if(!rules.peaceful) { waves.update(dt); brambleGrows(dt); roots.update(dt) }
      pursue(dt)
    }
    sky.update(dt)
    if(warmthDirty) rebuildWarmth()
    networks.update(dt)
    updateFields(dt)

    val wet = waterOpen
    val cold = frozen
    var i = 0
    while(i < buildings.size) {
      val b = buildings(i)
      if(b.alive) {
        b.hitAgo += dt
        b.flooded = wet && b.block.chills && !b.block.onWater && floorAt(b.x, b.y) == Floor.Shallow
        b.condition = if(b.flooded || b.stopped) 0f else (if(cold && b.block.chills && !b.warm) 0.5f else 1f) * (if(b.hurriedUntil >= time) b.hurry else 1f)
        b.update(dt)
      }
      i += 1
    }
    if(removedAny) {
      removedAny = false
      buildings.filterInPlace(_.alive)
      domes.filterInPlace(_.alive)
    }
    fires.update(dt)

    rebuildGrid()
    i = 0
    while(i < creatures.size) {
      val c = creatures(i)
      if(c.alive) {
        c.beforeX = c.x
        c.beforeY = c.y
        c.update(dt)
      }
      i += 1
    }
    separate()
    shots.update(dt)
    creatures.filterInPlace(_.alive)

    updateLantern(dt)
    builder.update(dt)

    ledger.update(dt)
    rateClock += dt
    if(rateClock >= 1f) {
      rateClock -= 1f
      i = 0
      while(i < rates.length) {
        rates(i) += (recent(i) * 60f - rates(i)) * 0.06f
        if(rates(i) < 0.05f) rates(i) = 0f
        recent(i) = 0f
        i += 1
      }
    }
  }

  // ------------------------------------------------------- the lamplighter

  /** The tile the lamplighter digs at, or -1. */
  var mining = -1
  private var mined = 0f
  private var dug = false
  var cargo: Item = null
  var cargoAmount = 0
  val CargoLimit = 40

  def canMine(tile: Int): Boolean = {
    val o = oreTile(tile)
    o >= 0 && lantern != null && Items.all(o).hardness <= 2 && builtOn(tile) == null && solids(tile) == 0 && (!Floor.all(floors(tile)).water || dry) &&
      dist2(lantern.x, lantern.y, (tile % width + 0.5f) * Tile, (tile / width + 0.5f) * Tile) < sqr(MineReach * Tile)
  }
  private def sqr(v: Float): Float = v * v

  private def mine(dt: Float): Unit = if(mining < 0) {
    // What was dug goes to the Hearth, if it is close. What was taken out of it does not.
    if(dug) { val _ = stow() }
    dug = false
  } else {
    dug = true
    if(!canMine(mining)) mining = -1
    else {
      val item = Items.all(oreTile(mining))
      if(cargoAmount > 0 && !(cargo eq item)) stow()
      mined += dt * (1f + (if(hearth == null) 0 else hearth.tier) * 0.5f)
      if(mined >= 0.55f * item.hardness && (cargoAmount < CargoLimit || cargoAmount == 0)) {
        mined = 0f
        cargo = item
        cargoAmount += 1
        gathered(item, 1)
        sound(SoundKind.Dig, lantern.x, lantern.y)
        fx(FxKind.Hit, (mining % width + 0.5f) * Tile, (mining / width + 0.5f) * Tile, -Pi / 2f, item.color)
      }
      if(cargoAmount >= 10) stow()
      if(cargoAmount >= CargoLimit) mining = -1
    }
  }
  /** Puts what is carried into the Hearth, when it is close. */
  def stow(): Boolean =
    if(hearth != null && lantern != null && cargoAmount > 0 && dist2(lantern.x, lantern.y, hearth.cx, hearth.cy) < sqr(12f * Tile)) {
      hearth.stock.add(cargo, cargoAmount)
      delivered(cargo, cargoAmount)
      line(FxKind.Leaf, lantern.x, lantern.y, hearth.cx, hearth.cy, 0f, cargo.color)
      cargoAmount = 0
      true
    } else false
  def nearLantern(b: Building): Boolean = lantern != null && dist2(lantern.x, lantern.y, b.cx, b.cy) < sqr(HandReach * Tile)
  /** Gives what is carried to a building, as much as it takes. */
  def handOver(b: Building): Int = {
    var n = 0
    if(cargoAmount > 0 && nearLantern(b)) b match {
      case _: HearthBuilding => n = cargoAmount; if(!stow()) n = 0
      case _: BeltBuilding | _: SplitterBuilding | _: CrossingBuilding | _: SorterBuilding | _: SpillwayBuilding => ()
      case _ =>
        while(cargoAmount > 0 && b.accepts(cargo, b)) { b.receive(cargo, b); cargoAmount -= 1; n += 1 }
    }
    if(n > 0) sound(SoundKind.Place, b.cx, b.cy)
    n
  }
  /** Takes goods out of a store or the Hearth. */
  def takeFrom(b: Building, item: Item): Int =
    if(!nearLantern(b) || b.stock == null) 0
    else {
      if(cargoAmount > 0 && !(cargo eq item)) { val _ = stow(); cargoAmount = 0 }
      val n = scala.math.min(b.stock(item), CargoLimit - cargoAmount)
      if(n > 0) {
        b.stock.remove(item, n)
        cargo = item
        cargoAmount += n
        sound(SoundKind.Place, b.cx, b.cy)
      }
      n
    }

  private def updateLantern(dt: Float): Unit =
    if(lantern == null && hearth != null && hearth.alive) {
      lanternRespawn -= dt
      mining = -1
      if(lanternRespawn <= 0f) lantern = spawn(UnitKinds.lanterns(hearth.tier), Player, hearth.cx, hearth.cy)
    } else if(lantern != null) {
      if(lantern.hitAgo > 4f && lantern.hp < lantern.maxHp) lantern.hp = scala.math.min(lantern.maxHp, lantern.hp + dt * 20f)
      mine(dt)
    }

  // ------------------------------------------------------------ the Bramble

  // Hearts put out spires and hedges around them, most of all in spring.
  private var growth = 40f
  private def brambleGrows(dt: Float): Unit = {
    growth -= dt * (if(season == Season.Spring) 2f else if(season == Season.Winter) 0.5f else 1f) * rules.threat
    if(growth <= 0f) {
      growth = 34f
      nests.foreach { nest =>
        val own = buildings.count(b => b.alive && b.team == Bramble && !b.isInstanceOf[NestBuilding] && dist2(b.cx, b.cy, nest.cx, nest.cy) < sqr(12f * Tile))
        if(own < 6 + year * 6) {
          val spire = rng.chance(0.25f)
          val size = if(spire) 2 else 1
          var tries = 0
          var done = false
          while(!done && tries < 14) {
            tries += 1
            val a = rng.float() * Tau
            val d = rng.range(3.5f, if(spire) 7f else 9f)
            val x = scala.math.round(nest.cx / Tile + cos(a) * d)
            val y = scala.math.round(nest.cy / Tile + sin(a) * d)
            val open = (0 until size).forall(j => (0 until size).forall(i => inside(x + i, y + j) && solidAt(x + i, y + j) == Solid.Open && !floorAt(x + i, y + j).water && buildingAt(x + i, y + j) == null))
            if(open) {
              val b = place(if(spire) Blocks.Thorn else Blocks.Hedge, x, y, 0, Bramble)
              fx(FxKind.Puff, b.cx, b.cy, 0f, 0)
              done = true
            }
          }
        }
      }
    }
  }

  // Creatures of a kind of movement do not overlap.
  private def separate(): Unit = {
    var i = 0
    while(i < creatures.size) {
      val a = creatures(i)
      if(a.alive && !UnitKinds.isLantern(a.kind)) {
        val ax = a.x
        val ay = a.y
        scan(ax, ay, a.kind.radius) { b =>
          if(b.id < a.id && b.kind.flies == a.kind.flies && !UnitKinds.isLantern(b.kind)) {
            val reach = (a.kind.radius + b.kind.radius) * 0.85f
            val dx = a.x - b.x
            val dy = a.y - b.y
            val d2 = dx * dx + dy * dy
            if(d2 < reach * reach) {
              val d = sqrt(d2)
              val nx = if(d > 0.01f) dx / d else 1f
              val ny = if(d > 0.01f) dy / d else 0f
              val push = (reach - d) * 4f
              val total = a.mass + b.mass
              a.push(nx * push * b.mass / total, ny * push * b.mass / total)
              b.push(-nx * push * a.mass / total, -ny * push * a.mass / total)
            }
          }
        }
      }
      i += 1
    }
  }
}

object Valley {
  /** Size of a tile, in world units. */
  val Tile = 32
  val Player = 0
  val Bramble = 1
  /** Among the seams: a vent. */
  val Vent: Byte = 100
  val MineReach = 5f
  val HandReach = 8f
  val NestClearance = 11f
  private val Cell = 96
  private val MaxRadius = 40f

  /** Whether a building hands goods to belts next to it by itself. */
  def offloads(b: Building): Boolean = b match {
    case _: DrillBuilding | _: TapBuilding | _: GardenBuilding | _: CatapultBuilding | _: CrafterBuilding | _: SplitterBuilding | _: CrossingBuilding | _: SorterBuilding
        | _: SpillwayBuilding | _: ZiplineBuilding | _: HatchBuilding => true
    case _ => false
  }
}
