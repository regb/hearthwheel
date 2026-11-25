package hearthwheel

import org.scalatest.funsuite.AnyFunSuite
import hearthwheel.model._

class ModelSuite extends AnyFunSuite {
  private val Dt = 1f / 60f
  private val quiet = Rules("Test", "", 64, 5L, Goal.Endless, nests = 1, peaceful = true, free = true, calm = true)

  /** A bare meadow with a Hearth in a corner, to lay things out on. */
  private def field(rules: Rules = quiet, size: Int = 64): Valley = {
    val w = new Valley(size, size, rules)
    java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
    w.place(Blocks.Hearth, 2, 2, 0, Valley.Player)
    w.learned ++= Blocks.all
    w
  }
  private def run(w: Valley, seconds: Float): Unit = {
    var i = 0
    val n = (seconds * 60f).toInt
    while(i < n) { w.update(Dt); i += 1 }
  }
  private def seam(w: Valley, item: Item, x: Int, y: Int, size: Int): Unit =
    for(dy <- 0 until size; dx <- 0 until size) w.ores((y + dy) * w.width + x + dx) = item.id.toByte

  // A belt from a point into the Hearth's right side.
  private def beltToHearth(w: Valley, fromX: Int, y: Int): Unit =
    for(x <- 5 to fromX) { val _ = w.place(Blocks.Belt, x, y, 2, Valley.Player) }

  test("a drill and a belt bring copper to the Hearth") {
    val w = field()
    seam(w, Items.Copper, 10, 2, 2)
    w.place(Blocks.Drill, 10, 2, 0, Valley.Player)
    beltToHearth(w, 9, 3)
    val before = w.hearthStock(Items.Copper)
    run(w, 60f)
    val got = w.hearthStock(Items.Copper) - before
    // One every 2.4 seconds, less what is still on the way.
    assert(got >= 18 && got <= 26, got)
  }

  test("belts keep their items in order and apart, and pass them all on") {
    val w = field()
    for(x <- 5 to 20) { val _ = w.place(Blocks.Belt, x, 3, 2, Valley.Player) }
    val store = w.place(Blocks.Storehouse, 22, 2, 0, Valley.Player)
    store.stock.add(Items.Tin, 100)
    w.place(Blocks.Hatch, 21, 3, 0, Valley.Player)
    run(w, 40f)
    assert(store.stock(Items.Tin) == 0)
    assert(w.hearthStock(Items.Tin) == 100)
    w.buildings.foreach {
      case b: BeltBuilding => assert(b.count == 0)
      case _ => ()
    }
  }

  test("a full belt stops, with a gap between items") {
    val w = field()
    val store = w.place(Blocks.Storehouse, 30, 30, 0, Valley.Player)
    store.stock.add(Items.Coal, 200)
    w.place(Blocks.Hatch, 32, 30, 0, Valley.Player)
    for(x <- 33 to 36) { val _ = w.place(Blocks.Belt, x, 30, 0, Valley.Player) }
    run(w, 10f)
    val belts = (33 to 36).map(x => w.buildingAt(x, 30).asInstanceOf[BeltBuilding])
    assert(belts.forall(_.count == 4))
    belts.foreach(b => for(i <- 1 until 4) assert(b.at(i - 1) - b.at(i) >= 0.24f))
    assert(store.stock(Items.Coal) == 200 - 16)
  }

  test("sorters, spillways, crossings and splitters send goods where they should") {
    val w = field()
    val source = w.place(Blocks.Storehouse, 20, 20, 0, Valley.Player)
    source.stock.add(Items.Copper, 30)
    source.stock.add(Items.Tin, 30)
    w.place(Blocks.Hatch, 22, 20, 0, Valley.Player)
    w.place(Blocks.Belt, 23, 20, 0, Valley.Player)
    w.place(Blocks.Sorter, 24, 20, 0, Valley.Player, config = Items.Copper.id)
    w.place(Blocks.Belt, 25, 20, 0, Valley.Player)
    w.place(Blocks.Crossing, 26, 20, 0, Valley.Player)
    w.place(Blocks.Belt, 27, 20, 0, Valley.Player)
    val ahead = w.place(Blocks.Storehouse, 28, 20, 0, Valley.Player)
    w.place(Blocks.Belt, 24, 21, 1, Valley.Player)
    val aside = w.place(Blocks.Storehouse, 24, 22, 0, Valley.Player)
    run(w, 30f)
    assert(ahead.stock(Items.Copper) == 30 && ahead.stock(Items.Tin) == 0)
    assert(aside.stock(Items.Tin) == 30 && aside.stock(Items.Copper) == 0)
  }

  test("ziplines carry goods over what is in between") {
    val w = field()
    val source = w.place(Blocks.Storehouse, 20, 20, 0, Valley.Player)
    source.stock.add(Items.Glass, 40)
    w.place(Blocks.Hatch, 22, 20, 0, Valley.Player)
    w.place(Blocks.Zipline, 23, 20, 0, Valley.Player)
    w.place(Blocks.Wall, 24, 20, 0, Valley.Player)
    w.place(Blocks.Zipline, 27, 20, 0, Valley.Player)
    w.buildingAt(23, 20).configure(20 * w.width + 27)
    val end = w.place(Blocks.Storehouse, 28, 20, 0, Valley.Player)
    run(w, 20f)
    assert(end.stock(Items.Glass) == 40)
  }

  test("a powered workshop needs its pylon, and slows down with too little power") {
    val w = field()
    val works = w.place(Blocks.Gearworks, 20, 20, 0, Valley.Player).asInstanceOf[CrafterBuilding]
    works.stock.add(Items.Bronze, 8)
    run(w, 3f)
    assert(works.stock(Items.Gear) == 0)
    w.place(Blocks.Windmill, 26, 20, 0, Valley.Player)
    w.place(Blocks.Pylon, 24, 20, 0, Valley.Player)
    run(w, 1f)
    assert(works.power != null && works.power.satisfaction > 0.4f && works.power.satisfaction < 1f)
    run(w, 12f)
    assert(works.stock(Items.Gear) >= 3)
    w.removeBuilding(w.buildingAt(24, 20))
    run(w, 0.1f)
    assert(works.powerShare == 0f)
  }

  test("flywheels fill with what is left over, and give it back") {
    val w = field()
    w.place(Blocks.Windmill, 26, 20, 0, Valley.Player)
    val wheel = w.place(Blocks.Flywheel, 22, 20, 0, Valley.Player)
    w.place(Blocks.Pylon, 24, 22, 0, Valley.Player)
    run(w, 20f)
    val stored = wheel.power.stored
    assert(stored > 10f)
    w.removeBuilding(w.buildingAt(26, 20))
    run(w, 0.1f)
    assert(scala.math.abs(wheel.power.stored - stored) < 2f)
  }

  test("water runs through pipes, and pumps freeze in winter unless they are warm") {
    val w = field()
    for(x <- 20 to 22) w.floors(20 * w.width + x) = Floor.Deep.ordinal.toByte
    w.place(Blocks.Pump, 22, 20, 0, Valley.Player)
    for(x <- 23 to 26) { val _ = w.place(Blocks.Pipe, x, 20, 0, Valley.Player) }
    seam(w, Items.Coal, 27, 20, 3)
    val auger = w.place(Blocks.Auger, 27, 20, 0, Valley.Player)
    run(w, 2f)
    assert(auger.tanks.nonEmpty && auger.waterShare > 0.9f)
    w.seasonIndex = Season.Winter.ordinal
    w.seasonTime = 0f
    run(w, 30f)
    assert(auger.tanks.head.supplied == 0f)
    w.place(Blocks.Brazier, 23, 22, 0, Valley.Player).receive(Items.Coal, null)
    run(w, 1f)
    assert(auger.tanks.head.supplied > 0f)
  }

  test("shallows are only for building on in summer, and drown machines after") {
    val w = field()
    for(y <- 20 to 24; x <- 20 to 24) w.floors(y * w.width + x) = Floor.Shallow.ordinal.toByte
    seam(w, Items.Copper, 20, 20, 2)
    assert(w.placementProblem(Blocks.Drill, 20, 20, 0) != null)
    assert(w.placementProblem(Blocks.Belt, 20, 20, 0) == null)
    w.seasonIndex = Season.Summer.ordinal
    assert(w.placementProblem(Blocks.Drill, 20, 20, 0) == null)
    val drill = w.place(Blocks.Drill, 20, 20, 0, Valley.Player)
    run(w, 1f)
    assert(drill.condition == 1f)
    w.seasonIndex = Season.Autumn.ordinal
    w.seasonTime = 0f
    run(w, 0.1f)
    assert(drill.flooded && drill.condition == 0f)
  }

  test("winter slows cold machines, and ice carries the Bramble until it breaks") {
    val rules = quiet.copy(peaceful = false, free = true, seasons = Vector(1000f, 1000f, 1000f, 5f), startSeason = 3, waveSeconds = 9999f, firstWave = 9999f)
    val w = field(rules)
    for(y <- 0 until 64; x <- 30 to 36) w.floors(y * w.width + x) = Floor.Deep.ordinal.toByte
    w.groundField.invalidate()
    w.groundField.finish()
    assert(w.frozen)
    assert(w.groundField.reaches(50, 30))
    val burr = w.spawn(UnitKinds.Stumper, Valley.Bramble, 33.5f * 32, 30.5f * 32)
    val smelter = w.place(Blocks.Smelter, 50, 50, 0, Valley.Player)
    run(w, 2f)
    assert(smelter.condition == 0.5f)
    assert(burr.alive)
    run(w, 4f)
    assert(w.season == Season.Spring)
    assert(!burr.alive)
    run(w, 2f)
    assert(!w.groundField.reaches(50, 30))
    assert(smelter.condition == 1f)
  }

  test("turrets shoot what comes, and need something to shoot") {
    val w = field()
    val turret = w.place(Blocks.Slinger, 20, 20, 0, Valley.Player).asInstanceOf[TurretBuilding]
    val burr = w.spawn(UnitKinds.Nettle, Valley.Bramble, 24.5f * 32, 20.5f * 32)
    run(w, 1f)
    assert(burr.hp > burr.maxHp - 1f)
    for(_ <- 0 until 10) turret.receive(Items.Flint, null)
    run(w, 8f)
    assert(!burr.alive)
  }

  test("every turret hurts its foes") {
    val w = field(quiet.copy(startSeason = 1))
    val turrets = Vector(Blocks.Slinger, Blocks.Emberpot, Blocks.Boltcaster, Blocks.Bell, Blocks.Lance, Blocks.Mister)
    turrets.zipWithIndex.foreach { case (block, i) =>
      val t = w.place(block, 10, 8 + i * 9, 0, Valley.Player).asInstanceOf[TurretBuilding]
      block.role match {
        case r: Role.Turret => r.ammo.foreach { case (item, _) => for(_ <- 0 until 5) t.receive(item, null) }
        case _ => ()
      }
      if(block.usesPower) {
        for(k <- 0 until 6) { val _ = w.place(Blocks.SunLens, 1 + 2 * (k % 3), 12 + 2 * (k / 3) + i * 9, 0, Valley.Player) }
        w.place(Blocks.Pylon, 8, 11 + i * 9, 0, Valley.Player)
      }
      if(block.waterUse > 0f) {
        w.floors((8 + i * 9) * w.width + 8) = Floor.Deep.ordinal.toByte
        w.place(Blocks.Pump, 8, 8 + i * 9, 0, Valley.Player)
        w.place(Blocks.Pipe, 9, 8 + i * 9, 0, Valley.Player)
      }
    }
    val foes = turrets.indices.map(i => w.spawn(UnitKinds.Golem, Valley.Bramble, 14f * 32, (9.2f + i * 9) * 32))
    foes.foreach(_.frozen = 1000f)
    run(w, 6f)
    foes.zip(turrets).foreach { case (f, t) => assert(f.hp < f.maxHp - 5f, t.key) }
  }

  test("turrets in reach wear a Briar Heart down, and burning it out can win") {
    val w = field(quiet.copy(goal = Goal.Cleanse))
    w.place(Blocks.Nest, 40, 20, 0, Valley.Bramble)
    val t = w.place(Blocks.Boltcaster, 29, 20, 0, Valley.Player).asInstanceOf[TurretBuilding]
    var seconds = 0
    while(w.outcome == Outcome.Playing && seconds < 400) {
      while(t.accepts(Items.Gear, null)) t.receive(Items.Gear, null)
      run(w, 2f)
      seconds += 2
    }
    assert(w.outcome == Outcome.Won)
  }

  test("the Bramble finds the Hearth, through walls if it must") {
    val rules = quiet.copy(waveSeconds = 9999f, firstWave = 9999f)
    val w = field(rules)
    w.builder.paused = true
    // Walled in all around.
    for(i <- 0 to 12) { val _ = w.place(Blocks.Wall, 12, i, 0, Valley.Player); val _ = w.place(Blocks.Wall, i, 12, 0, Valley.Player) }
    for(_ <- 0 until 4) { val _ = w.spawn(UnitKinds.Stumper, Valley.Bramble, 40 * 32f, 6 * 32f) }
    run(w, 60f)
    assert(w.lost > 0)
    assert(w.hearth.hp < w.hearth.maxHp)
  }

  test("the lamplighter builds what is planned, pays for it, and takes it down again") {
    val w = field(quiet.copy(free = false))
    w.learned ++= Blocks.all
    w.hearth.stock.add(Items.Copper, 100)
    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, 200f, 200f)
    w.builder.add(Plan(Blocks.Slinger, 8, 8, 0))
    run(w, 3f)
    assert(w.buildingAt(8, 8) != null && (w.buildingAt(8, 8).block eq Blocks.Slinger))
    assert(w.hearthStock(Items.Copper) == 100 - 32)
    w.builder.add(Plan(Blocks.Slinger, 8, 8, 0, remove = true))
    run(w, 3f)
    assert(w.buildingAt(8, 8) == null)
    assert(w.hearthStock(Items.Copper) >= 100 - 8)
    // Without the goods the site waits for them.
    w.builder.add(Plan(Blocks.Smelter, 8, 8, 0))
    run(w, 3f)
    assert(w.buildingAt(8, 8).isInstanceOf[SiteBuilding] && w.builder.starved)
    w.hearth.stock.add(Items.Flint, 50)
    run(w, 4f)
    assert(w.buildingAt(8, 8).block eq Blocks.Smelter)
  }

  test("the Great Wheel takes each season's goods in that season, and wakes the Rimewood") {
    val w = field(quiet.copy(goal = Goal.Wheel))
    val wheel = w.place(Blocks.GreatWheel, 30, 30, 0, Valley.Player).asInstanceOf[GreatWheelBuilding]
    assert(wheel.accepts(Items.Resin, null) && !wheel.accepts(Items.Glass, null))
    for(s <- 0 until 4) {
      w.seasonIndex = s
      Blocks.spokes(s).foreach(stack => for(_ <- 0 until stack.amount) { assert(wheel.accepts(stack.item, null)); wheel.receive(stack.item, null) })
    }
    assert(wheel.complete && w.longWinter && w.season == Season.Winter)
    // The boss comes even though there is no nest here.
    var i = 0
    while(i < 600 && !w.creatures.exists(_.kind eq UnitKinds.Rimewood)) { w.waves.update(Dt); i += 1 }
    val boss = w.creatures.find(_.kind eq UnitKinds.Rimewood).get
    boss.hurt(1e6f, pierceArmor = true)
    assert(w.outcome == Outcome.Won)
  }

  test("every valley can be made, and the Bramble can reach its Hearth some time of the year") {
    Valleys.all.foreach { rules =>
      val w = MapGen.generate(rules)
      assert(w.hearth != null && w.nests.size == rules.nests, rules.name)
      w.seasonIndex = Season.Winter.ordinal
      w.groundField.invalidate()
      w.groundField.finish()
      w.nests.foreach { nest =>
        val x = nest.x - 1
        val y = nest.y + 1
        assert(w.climbField.reaches(x, y), rules.name)
      }
      val again = MapGen.generate(rules)
      assert(java.util.Arrays.equals(w.floors, again.floors) && java.util.Arrays.equals(w.ores, again.ores), rules.name)
    }
  }

  test("many seeds make valleys") {
    var failed = Vector.empty[Long]
    for(seed <- 1L to 25L) {
      try { val _ = MapGen.generate(quiet.copy(seed = seed, size = 96, nests = 2, peaceful = false)) }
      catch { case _: IllegalStateException => failed :+= seed }
    }
    assert(failed.isEmpty)
  }

  test("a save restores the world, which then goes the same way") {
    val w = MapGen.generate(Valleys.all(0))
    w.hearth.stock.add(Items.Copper, 500)
    val (hx, hy) = (w.hearth.x, w.hearth.y)
    for(i <- 0 until 6) { val _ = w.place(Blocks.Belt, hx + 3 + i, hy + 1, 2, Valley.Player) }
    w.place(Blocks.Pylon, hx - 3, hy, 0, Valley.Player)
    w.place(Blocks.Windmill, hx - 6, hy, 0, Valley.Player)
    w.place(Blocks.Slinger, hx, hy + 5, 0, Valley.Player)
    w.builder.add(Plan(Blocks.Wall, hx + 6, hy + 6, 0))
    run(w, 5f)
    val text = Codec.save(w)
    val back = Codec.load(text, w.rules).get
    assert(Codec.save(back) == text)
    run(back, 5f)
    assert(back.buildings.exists(_.block eq Blocks.Wall))
    assert(Codec.load(text.take(text.length / 2), w.rules).isFailure)
    assert(Codec.load("", w.rules).isFailure)
  }

  test("nothing but the Hearth's end ends an endless game, and an undefended Hearth does end") {
    val w = MapGen.generate(Valleys.all(0).copy(goal = Goal.Endless, threat = 3f, firstWave = 5f, waveSeconds = 20f, seasons = Vector(60f, 60f, 60f, 60f)))
    var seconds = 0
    while(w.outcome == Outcome.Playing && seconds < 1500) { run(w, 10f); seconds += 10 }
    assert(w.outcome == Outcome.Lost, s"after $seconds seconds, wave ${w.waves.number}, ${w.creatures.size} creatures")
  }

  // ------------------------------------------------------------ later works

  private def power(w: Valley, x: Int, y: Int, n: Int): Unit = {
    for(k <- 0 until n) { val b = w.place(Blocks.SunCourt, x + (k % 4) * 3, y + (k / 4) * 3, 0, Valley.Player); val _ = b }
    for(k <- 0 until 2) { val _ = w.place(Blocks.TallPylon, x + 12, y + k * 2, 0, Valley.Player) }
  }
  private def well(w: Valley, x: Int, y: Int): Unit = {
    w.floors(y * w.width + x) = Floor.Deep.ordinal.toByte
    val _ = w.place(Blocks.Pump, x, y, 0, Valley.Player)
  }
  private val sunny = quiet.copy(startSeason = 1)

  test("a press fills pipes with oil, which does not mix with water") {
    val w = field(sunny)
    power(w, 10, 10, 4)
    val press = w.place(Blocks.Press, 23, 15, 0, Valley.Player)
    for(_ <- 0 until 8) press.receive(Items.Resin, null)
    for(x <- 25 to 30) { val _ = w.place(Blocks.Pipe, x, 15, 0, Valley.Player) }
    run(w, 4f)
    val pipe = w.buildingAt(28, 15)
    assert((pipe.fluid.fluid eq Fluid.Oil) && pipe.fluid.stored > 5f)
    well(w, 31, 15)
    run(w, 2f)
    assert(pipe.fluid.fluid eq Fluid.Oil)
  }

  test("culverts carry water under what is in between") {
    val w = field()
    well(w, 20, 20)
    w.place(Blocks.Culvert, 21, 20, 0, Valley.Player)
    w.place(Blocks.Wall, 22, 20, 0, Valley.Player)
    w.place(Blocks.Culvert, 25, 20, 0, Valley.Player)
    w.buildingAt(21, 20).configure(20 * w.width + 25)
    val tank = w.place(Blocks.Cistern, 26, 20, 0, Valley.Player)
    run(w, 6f)
    assert(tank.fluid.stored > 30f && (tank.fluid.fluid eq Fluid.Water))
    w.removeBuilding(w.buildingAt(25, 20))
    run(w, 1f)
    assert(!tank.fluid.conduits.contains(w.buildingAt(21, 20)))
  }

  test("a chiller makes brine, and a cooled turret shoots faster") {
    val w = field(sunny)
    power(w, 8, 20, 4)
    well(w, 26, 26)
    w.place(Blocks.Chiller, 27, 26, 0, Valley.Player)
    for(x <- 29 to 33) { val _ = w.place(Blocks.Pipe, x, 26, 0, Valley.Player) }
    w.place(Blocks.Pylon, 25, 23, 0, Valley.Player)
    val cooled = w.place(Blocks.Slinger, 34, 26, 0, Valley.Player).asInstanceOf[TurretBuilding]
    val plain = w.place(Blocks.Slinger, 34, 29, 0, Valley.Player).asInstanceOf[TurretBuilding]
    Vector(cooled, plain).foreach { t => t.rounds(0) = 400; t.loaded = 0 }
    val foe = w.spawn(UnitKinds.Golem, Valley.Bramble, 38 * 32f, 28 * 32f)
    foe.frozen = 1000f
    foe.hp = 1e6f
    run(w, 12f)
    assert(cooled.coolant eq Fluid.Brine)
    val (a, b) = (400 - cooled.rounds(0), 400 - plain.rounds(0))
    assert(a > b * 1.3f, (a, b))
  }

  test("boilers want water and fuel, and reactors burst without brine") {
    val w = field()
    well(w, 20, 20)
    val boiler = w.place(Blocks.Boiler, 21, 20, 0, Valley.Player).asInstanceOf[BoilerBuilding]
    for(_ <- 0 until 6) boiler.receive(Items.Coal, null)
    run(w, 2f)
    assert(boiler.output > 5f)
    val reactor = w.place(Blocks.Reactor, 40, 40, 0, Valley.Player).asInstanceOf[ReactorBuilding]
    val wall = w.place(Blocks.Wall, 45, 41, 0, Valley.Player)
    for(_ <- 0 until 20) reactor.receive(Items.Ember, null)
    run(w, 5f)
    assert(reactor.alive && reactor.heat > 0.2f && reactor.powerOutput > 15f)
    run(w, 20f)
    assert(!reactor.alive && !wall.alive)
  }

  test("a ratchet lets stored power through one way") {
    val w = field()
    val full = w.place(Blocks.Flywheel, 20, 20, 0, Valley.Player)
    w.place(Blocks.Ratchet, 22, 20, 0, Valley.Player)
    val empty = w.place(Blocks.Flywheel, 23, 20, 0, Valley.Player)
    run(w, 0.1f)
    full.power.stored = 100f
    run(w, 10f)
    assert(empty.power.stored > 40f && full.power.stored > 40f)
    full.power.stored = 0f
    run(w, 5f)
    assert(full.power.stored < 1f)
  }

  test("an aegis stops shots until it breaks, and swatters swat them") {
    val w = field(sunny)
    power(w, 10, 10, 8)
    val aegis = w.place(Blocks.Aegis, 25, 14, 0, Valley.Player).asInstanceOf[AegisBuilding]
    val wall = w.place(Blocks.Wall, 27, 18, 0, Valley.Player)
    run(w, 12f)
    assert(aegis.open)
    val shot = UnitKinds.Nettle.arms.asInstanceOf[Arms.Shoots].shot
    def volley(): Unit = { val _ = w.shots.fire(shot, Valley.Bramble, 27.5f * 32, 26 * 32f, -scala.math.Pi.toFloat / 2f, 0f, 400f, true, true) }
    volley()
    run(w, 2f)
    assert(wall.hp == wall.maxHp)
    aegis.absorb(1e6f, 0f, 0f)
    run(w, 0.5f)
    volley()
    run(w, 2f)
    assert(wall.hp < wall.maxHp)
    val swatter = w.place(Blocks.Swatter, 25, 18, 0, Valley.Player)
    run(w, 1f)
    val before = wall.hp
    volley()
    run(w, 1.5f)
    assert(swatter.powerShare > 0.5f && wall.hp >= before)
  }

  test("a metronome hurries what is around it") {
    def made(hurried: Boolean): Int = {
      val w = field(sunny)
      power(w, 10, 10, 8)
      val mill = w.place(Blocks.Smelter, 24, 14, 0, Valley.Player).asInstanceOf[CrafterBuilding]
      for(_ <- 0 until 100) { mill.stock.add(Items.Copper, 2); mill.stock.add(Items.Tin, 1); mill.stock.add(Items.Coal, 1) }
      if(hurried) { val _ = w.place(Blocks.Metronome, 24, 17, 0, Valley.Player) }
      for(_ <- 0 until 10) { run(w, 2f); mill.stock.remove(Items.Bronze, 100) }
      w.gatheredCounts(Items.Bronze.id)
    }
    val (slow, fast) = (made(false), made(true))
    assert(fast >= slow * 1.3f, (slow, fast))
  }

  test("a longbow goes through a line of them, a grapnel reels flyers in, caltrops cut") {
    val w = field(sunny)
    power(w, 6, 6, 8)
    val bow = w.place(Blocks.Longbow, 20, 10, 0, Valley.Player).asInstanceOf[TurretBuilding]
    for(_ <- 0 until 4) bow.receive(Items.Steel, null)
    val line = (0 until 3).map(i => w.spawn(UnitKinds.Golem, Valley.Bramble, (30 + i * 2) * 32f, 11.5f * 32))
    line.foreach(_.frozen = 1000f)
    run(w, 4f)
    assert(line.forall(c => c.hp < c.maxHp - 100f))
    line.foreach(w.killCreature)
    val hook = w.place(Blocks.Grapnel, 20, 15, 0, Valley.Player)
    val kite = w.spawn(UnitKinds.Hornwing, Valley.Bramble, 30 * 32f, 16 * 32f)
    val far = MathUtil.dist(kite.x, kite.y, hook.cx, hook.cy)
    run(w, 3f)
    assert(!kite.alive || MathUtil.dist(kite.x, kite.y, hook.cx, hook.cy) < far)
    w.place(Blocks.Caltrop, 40, 40, 0, Valley.Player)
    val walker = w.spawn(UnitKinds.Stumper, Valley.Bramble, 40.5f * 32, 40.5f * 32)
    run(w, 1f)
    assert(walker.hp < walker.maxHp - 15f)
  }

  test("gates let one's own through and nobody else") {
    val w = field()
    w.place(Blocks.Gate, 20, 20, 0, Valley.Player)
    val own = w.spawn(UnitKinds.Beetle, Valley.Player, 500f, 500f)
    val foe = w.spawn(UnitKinds.Burr, Valley.Bramble, 500f, 500f)
    assert(!w.blocks(own, 20, 20) && w.blocks(foe, 20, 20))
  }

  test("switches and governors stop what they touch") {
    val w = field()
    seam(w, Items.Copper, 20, 20, 2)
    val drill = w.place(Blocks.Drill, 20, 20, 0, Valley.Player)
    val switch = w.place(Blocks.Switch, 22, 20, 0, Valley.Player)
    run(w, 1f)
    assert(drill.condition == 1f)
    switch.configure(0)
    run(w, 0.2f)
    assert(drill.condition == 0f)
    w.removeBuilding(switch)
    val governor = w.place(Blocks.Governor, 22, 20, 0, Valley.Player).asInstanceOf[GovernorBuilding]
    governor.choose(Watch.Below, Items.Copper, amount = 200)
    w.hearth.stock.remove(Items.Copper, 100000)
    run(w, 1f)
    assert(drill.condition == 1f)
    w.hearth.stock.add(Items.Copper, 300)
    run(w, 1f)
    assert(drill.condition == 0f)
    governor.choose(Watch.InSeason, season = Season.Spring)
    run(w, 1f)
    assert(drill.condition == 1f)
    val copy = GovernorBuilding.encode(Watch.Foes, 13, 3, 12)
    governor.configure(copy)
    assert(governor.watch == Watch.Foes && (governor.item eq Items.Powder) && governor.season == Season.Winter && governor.amount == 1200)
  }

  test("automata are made better in a refit yard, all the way up") {
    val w = field(sunny)
    power(w, 6, 6, 8)
    var kind = UnitKinds.Beetle
    val yards = Vector(Blocks.Refit, Blocks.Foundry, Blocks.StarForge)
    val expected = Vector(UnitKinds.Stag, UnitKinds.Golem, UnitKinds.Colossus)
    yards.zip(expected).foreach { case (block, next) =>
      val yard = w.place(block, 21, 10, 0, Valley.Player).asInstanceOf[RefitBuilding]
      for(_ <- 0 until 400; s <- yard.role.cost) if(yard.accepts(s.item, null)) yard.receive(s.item, null)
      val c = w.spawn(kind, Valley.Player, yard.cx, yard.cy + (block.size / 2f + 1.5f) * 32)
      run(w, yard.role.seconds * 3f + 5f)
      assert(!c.alive && w.creatures.exists(_.kind eq next), block.key)
      kind = next
      w.creatures.filter(x => !UnitKinds.isLantern(x.kind)).foreach(_.alive = false)
      w.removeBuilding(yard)
      run(w, 0.1f)
    }
  }

  test("the Hearth grows, keeps its goods, and lights a greater lantern") {
    val w = field(quiet.copy(free = false))
    w.learned ++= Blocks.all
    Items.all.foreach(i => w.hearth.stock.add(i, 4000))
    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, 200f, 200f)
    val copper = w.hearthStock(Items.Copper)
    assert(w.placementProblem(Blocks.GreatHearth, 6, 6, 0) != null)
    w.builder.add(Plan(Blocks.GreatHearth, 2, 2, 0))
    run(w, 1f)
    assert(w.hearth.size == 4 && w.hearth.alive && w.outcome == Outcome.Playing)
    assert(w.hearthStock(Items.Copper) == copper - 1800)
    assert(w.lantern.kind eq UnitKinds.LanternGreat)
    assert(w.crewLimit == 24)
  }

  test("the lamplighter digs, carries, gives and takes") {
    val w = field(quiet.copy(free = false))
    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, 6 * 32f, 6 * 32f)
    seam(w, Items.Copper, 8, 8, 2)
    w.mining = 8 * w.width + 8
    run(w, 12f)
    assert(w.hearthStock(Items.Copper) >= 10)
    w.mining = -1
    val turret = w.place(Blocks.Slinger, 9, 5, 0, Valley.Player).asInstanceOf[TurretBuilding]
    w.hearth.stock.add(Items.Flint, 100)
    assert(w.takeFrom(w.hearth, Items.Flint) == 40)
    assert(w.handOver(turret) > 0 && turret.hasAmmo)
    val iron = 12 * w.width + 10
    w.ores(iron) = Items.Iron.id.toByte
    assert(!w.canMine(iron))
  }

  test("wrens dig what the Hearth lacks, and tenders mend") {
    val w = field()
    seam(w, Items.Copper, 10, 10, 3)
    seam(w, Items.Tin, 14, 10, 3)
    w.hearth.stock.add(Items.Copper, 500)
    val before = w.hearthStock(Items.Tin)
    for(_ <- 0 until 2) { val _ = w.spawn(UnitKinds.Wren, Valley.Player, 300f, 300f) }
    run(w, 60f)
    assert(w.hearthStock(Items.Tin) > before)
    val wall = w.place(Blocks.Wall, 30, 30, 0, Valley.Player)
    wall.hp = 50f
    w.spawn(UnitKinds.Tender, Valley.Player, wall.cx, wall.cy)
    run(w, 5f)
    assert(wall.hp > 100f)
  }

  test("fire spreads among what burns, and misters put it out") {
    val w = field(quiet.copy(startSeason = 1))
    val stores = (0 until 4).map { i =>
      val b = w.place(Blocks.Storehouse, 20 + i * 2, 20, 0, Valley.Player)
      b.stock.add(Items.Coal, 200)
      b
    }
    w.fires.start(20, 20)
    run(w, 25f)
    assert(stores.count(b => !b.alive || b.hp < b.maxHp) >= 2)
    val v = field(quiet.copy(startSeason = 0))
    well(v, 20, 24)
    v.place(Blocks.Pipe, 21, 24, 0, Valley.Player)
    v.place(Blocks.Mister, 22, 24, 0, Valley.Player)
    val coal = v.place(Blocks.Storehouse, 22, 21, 0, Valley.Player)
    coal.stock.add(Items.Coal, 250)
    run(v, 2f)
    v.fires.start(22, 21)
    run(v, 4f)
    assert(v.fires.burning.isEmpty && coal.alive)
  }

  test("catapults throw goods across the valley, gardens grow, wagons leave") {
    val w = field(sunny)
    power(w, 6, 6, 8)
    val from = w.place(Blocks.Catapult, 20, 10, 0, Valley.Player).asInstanceOf[CatapultBuilding]
    val to = w.place(Blocks.Catapult, 45, 30, 0, Valley.Player).asInstanceOf[CatapultBuilding]
    from.configure(30 * w.width + 45)
    val store = w.place(Blocks.Storehouse, 48, 30, 0, Valley.Player)
    for(_ <- 0 until 50) from.receive(Items.Gear, null)
    run(w, 20f)
    assert(store.stock(Items.Gear) + to.stock(Items.Gear) == 50 && store.stock(Items.Gear) > 10)
    val garden = w.place(Blocks.Garden, 20, 40, 0, Valley.Player)
    val basket = w.place(Blocks.Storehouse, 23, 40, 0, Valley.Player)
    run(w, 40f)
    assert(garden.alive && basket.stock(Items.Cloth) >= 3)
    val post = w.place(Blocks.WagonPost, 40, 50, 0, Valley.Player)
    for(_ <- 0 until 110) post.receive(Items.Bronze, null)
    run(w, 15f)
    assert(w.sentOut(Items.Bronze.id) == 110)
  }

  test("hearts put out hedges, guardians lead, and later waves are shielded") {
    val w = MapGen.generate(Valleys.all(0).copy(goal = Goal.Endless, firstWave = 9999f, calm = true))
    val before = w.buildings.count(_.team == Valley.Bramble)
    run(w, 200f)
    assert(w.buildings.count(_.team == Valley.Bramble) > before)
    w.waves.number = 19
    w.waves.launchNow()
    run(w, 30f)
    val foes = w.creatures.filter(_.team == Valley.Bramble)
    assert(foes.exists(_.guardian) && foes.forall(_.maxShield > 0f))
  }

  test("a patient valley waits for the last foe, and skies turn") {
    val w = MapGen.generate(Valleys.all(0).copy(goal = Goal.Endless, patient = true, firstWave = 1f, waveSeconds = 5f))
    run(w, 3f)
    assert(w.waves.number == 1 && w.waves.held)
    run(w, 10f)
    assert(w.waves.number == 1)
    w.sky.begin(SkyEvent.Cloudburst)
    assert(w.sky.stream > 1f)
    w.seasonIndex = 1
    run(w, 0.2f)
    assert(w.sky.event == null)
  }

  test("a save keeps one of everything") {
    val w = field(sunny.copy(size = 200), 200)
    var x = 8
    var y = 10
    Blocks.all.filter(b => b.category.nonEmpty && !(b.role eq Role.Hearth)).foreach { b =>
      if(x + b.size > 180) { x = 8; y += 6 }
      w.place(b, x, y, 1, Valley.Player)
      x += b.size + 1
    }
    UnitKinds.all.filterNot(UnitKinds.isLantern).foreach { k => val c = w.spawn(k, Valley.Player, 3000f + k.id * 40f, 5500f); c.rallyX = c.x; c.rallyY = c.y }
    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, 200f, 200f)
    w.cargo = Items.Tin
    w.cargoAmount = 7
    w.fires.start(8, 10)
    run(w, 3f)
    val text = Codec.save(w)
    val back = Codec.load(text, w.rules).get
    assert(back.buildings.size == w.buildings.size && back.creatures.size == w.creatures.size)
    assert(back.cargoAmount == 7 && (back.cargo eq Items.Tin))
    run(back, 3f)
    assert(Codec.load(Codec.save(back), w.rules).isSuccess)
  }

  test("a belt that finds its own way goes straight over another one") {
    val w = field()
    for(y <- 10 to 50) { val _ = w.place(Blocks.Belt, 30, y, 1, Valley.Player) }
    val way = Builder.path(w, Blocks.Belt, 25, 30, 35, 30)
    assert(way.size == 11)
    val plans = Builder.belts(w, Blocks.Belt, way)
    assert(plans.count(_.block eq Blocks.Crossing) == 1)
    // Never along another one, nor into its side.
    val around = Builder.path(w, Blocks.Belt, 28, 48, 33, 52)
    around.foreach { case (x, y, d) => w.buildingAt(x, y) match {
      case b: BeltBuilding => assert(((b.rotation ^ d) & 1) == 1)
      case _ => ()
    } }
  }

  private val wild = quiet.copy(peaceful = false, timer = false, wild = true, free = true)

  test("roots creep out of a heart, gnaw at what is built, burn, and die with it") {
    val w = field(wild)
    val nest = w.place(Blocks.Nest, 40, 40, 0, Valley.Bramble)
    run(w, 120f)
    val spread = w.roots.size
    assert(spread > 40, spread)
    val fed = (nest -> w.roots.span) +: w.roots.pods.map(p => p -> w.roots.PodSpan)
    assert(w.roots.thick.indices.filter(i => w.roots.thick(i) > 0).forall(i => fed.exists { case (b, r) => MathUtil.dist(i % 64 * 32f, i / 64 * 32f, b.cx, b.cy) < (r + 3f) * 32f }))
    val (rx, ry) = w.roots.thick.indices.filter(i => w.roots.thick(i) > 0).map(i => (i % 64, i / 64)).maxBy { case (x, y) => MathUtil.dist(x * 32f, y * 32f, nest.cx, nest.cy) }
    assert(w.placementProblem(Blocks.Wall, rx, ry, 0) != null)
    // Fire takes them.
    w.explode((rx + 0.5f) * 32f, (ry + 0.5f) * 32f, 40f, 0f, Valley.Player, 3f, 0f, air = false, ground = true, 0)
    assert(!w.roots.at(rx, ry))
    val again = Codec.load(Codec.save(w), wild).get
    assert(again.roots.size == w.roots.size && java.util.Arrays.equals(again.roots.thick, w.roots.thick))
    w.roots.pods.foreach(w.destroyBuilding)
    w.destroyBuilding(nest)
    run(w, 120f)
    assert(w.roots.size < spread / 8, w.roots.size)
  }

  test("what stands in the way of roots is gnawed at") {
    val w = field(wild)
    w.place(Blocks.Nest, 40, 40, 0, Valley.Bramble)
    for(k <- 30 to 50) { val _ = w.place(Blocks.BronzeWall, 36, k, 0, Valley.Player) }
    run(w, 240f)
    assert(w.buildings.exists(b => b.team == Valley.Player && b.block.isWall && b.hp < b.maxHp) || w.lost > 0)
  }

  test("pods hatch a brood that keeps watch, and marches with every third wave") {
    val w = field(wild)
    w.place(Blocks.Nest, 50, 50, 0, Valley.Bramble)
    assert(w.roots.plant(40, 40))
    run(w, 100f)
    val brood = w.creatures.filter(_.guarding).toVector
    assert(brood.nonEmpty && brood.size <= w.roots.pods.map(_.maxBrood).sum)
    assert(brood.forall(c => MathUtil.dist(c.x, c.y, c.rallyX, c.rallyY) < 10f * 32f))
    assert(Codec.load(Codec.save(w), wild).get.creatures.count(_.guarding) == brood.size)
    w.waves.launchNow(); w.waves.launchNow()
    assert(brood.forall(_.guarding))
    w.waves.launchNow()
    assert(!brood.exists(_.guarding))
  }

  test("thistledown takes root where it lands, unless it is shot down") {
    val w = field(wild)
    def send(): Creature = { val c = w.spawn(UnitKinds.Thistledown, Valley.Bramble, 20f * 32f, 40f * 32f); c.rallyX = 26f * 32f; c.rallyY = 40f * 32f; c }
    val first = send()
    run(w, 20f)
    assert(!first.alive && w.roots.pods.size == 1)
    val second = send()
    w.killCreature(second)
    run(w, 20f)
    assert(w.roots.pods.size == 1)
  }

  private def lake(season: Int): Valley = {
    val w = field(quiet.copy(startSeason = season))
    for(y <- 20 to 40; x <- 10 to 50) w.floors(y * 64 + x) = (if(x < 30) Floor.Deep else Floor.Shallow).ordinal.toByte
    w
  }

  test("boats keep to the water, dry out in summer, and freeze in winter") {
    def sail(season: Int, kind: UnitKind, fromX: Int, toX: Int): Float = {
      val w = lake(season)
      val c = w.spawn(kind, Valley.Player, (fromX + 0.5f) * 32f, 30.5f * 32f)
      val (x, y) = w.standing(kind, (toX + 0.5f) * 32f, 30.5f * 32f)
      c.rallyX = x
      c.rallyY = y
      run(w, 12f)
      assert(w.afloat(w.tileIndex(c.tileX, c.tileY)) || season == 1)
      c.x / 32f - fromX
    }
    assert(sail(0, UnitKinds.Skiff, 14, 34) > 18f)
    // On a dry bed it does not move at all.
    assert(scala.math.abs(sail(1, UnitKinds.Skiff, 40, 14)) < 1.5f)
    assert(scala.math.abs(sail(3, UnitKinds.Skiff, 14, 28)) < 1.5f)
    assert(sail(3, UnitKinds.Icebreaker, 14, 28) > 5f)
    // Land is not for them: they go as near as they can.
    val w = lake(0)
    val (sx, _) = w.standing(UnitKinds.Skiff, 55f * 32f, 30f * 32f)
    assert(sx < 51f * 32f && sx > 49f * 32f)
  }

  test("a slipway launches skiffs onto the water, and a drydock makes them better") {
    val w = lake(0)
    assert(w.placementProblem(Blocks.Slipway, 5, 5, 0) == "Must be on water")
    val slip = w.place(Blocks.Slipway, 14, 22, 0, Valley.Player).asInstanceOf[WorkshopBuilding]
    UnitKinds.Skiff.cost.foreach(s => for(_ <- 0 until s.amount) slip.receive(s.item, null))
    val wind = (0 until 3).map(k => w.place(Blocks.Firebox, 5, 45 + k * 3, 0, Valley.Player))
    wind.foreach(b => for(_ <- 0 until 8) b.receive(Items.Coal, null))
    val pylon = w.place(Blocks.TallPylon, 9, 40, 0, Valley.Player)
    wind.foreach(w.link(pylon, _))
    w.link(pylon, slip)
    run(w, 40f)
    val skiff = w.creatures.find(_.kind eq UnitKinds.Skiff).get
    assert(w.afloat(w.tileIndex(skiff.tileX, skiff.tileY)))
    val dock = w.place(Blocks.Drydock, 20, 28, 0, Valley.Player).asInstanceOf[RefitBuilding]
    w.link(pylon, dock)
    dock.role.cost.foreach(s => for(_ <- 0 until s.amount) dock.receive(s.item, null))
    wind.foreach(b => for(_ <- 0 until 8) b.receive(Items.Coal, null))
    skiff.x = 19f * 32f
    skiff.y = 30f * 32f
    skiff.rallyX = skiff.x
    skiff.rallyY = skiff.y
    run(w, 60f)
    val cutter = w.creatures.find(_.kind eq UnitKinds.Cutter).get
    assert(w.afloat(w.tileIndex(cutter.tileX, cutter.tileY)))
  }

  private def poweredLoom(w: Valley): LoomBuilding = {
    val loom = w.place(Blocks.Loom, 20, 20, 0, Valley.Player).asInstanceOf[LoomBuilding]
    val wheel = w.place(Blocks.Windmill, 24, 20, 0, Valley.Player)
    val pylon = w.place(Blocks.Pylon, 22, 24, 0, Valley.Player)
    w.link(pylon, wheel)
    w.link(pylon, loom)
    loom
  }

  test("a loom stops, sorts and counts by its cards, and a board shows the count") {
    val w = field()
    val loom = poweredLoom(w)
    val turret = w.place(Blocks.Slinger, 18, 18, 0, Valley.Player)
    val sorter = w.place(Blocks.Sorter, 17, 20, 0, Valley.Player)
    val sign = w.place(Blocks.Sign, 18, 24, 0, Valley.Player).asInstanceOf[SignBuilding]
    assert(loom.toggle(turret) && loom.toggle(sorter))
    assert(!loom.toggle(w.place(Blocks.Wall, 60, 60, 0, Valley.Player)))
    val copper = w.hearthStock(Items.Copper)
    loom.cards = Vector(
      Card(Sense.Goods, Items.Copper.id, 1, copper + 10, Doing.Stop, 0, 0),
      Card(Sense.Season, 0, 2, 1, Doing.Choose, 1, Items.Tin.id),
      Card(Sense.Goods, Items.Copper.id, 1, 0, Doing.Copy, 2, 0),
      Card(Sense.Always, 0, 0, 0, Doing.Add, 1, 1)
    )
    sign.configure(2)
    run(w, 3f)
    assert(!turret.stopped && sorter.config == Items.Tin.id && sign.shown == copper)
    assert(w.drums(1) >= 4 && w.drums(1) <= 7)
    w.hearth.stock.add(Items.Copper, 50)
    run(w, 2f)
    assert(turret.stopped && sign.shown == copper + 50)
    // It all comes back out of a save, links too.
    val again = Codec.load(Codec.save(w), quiet).get
    val l = again.buildings.collectFirst { case l: LoomBuilding => l }.get
    assert(l.cards == loom.cards && l.tied(0).block == Blocks.Slinger && again.drums.toVector == w.drums.toVector)
    // And what is linked may be rebuilt.
    w.destroyBuilding(turret)
    run(w, 1f)
    val other = w.place(Blocks.Emberpot, 18, 18, 0, Valley.Player)
    run(w, 1f)
    assert(other.stopped)
  }

  test("layouts keep what looms were told, and nonsense is turned away") {
    val w = field()
    val loom = poweredLoom(w)
    loom.cards = Vector(Card.blank, Card(Sense.Foes, 0, 1, 3, Doing.Ring, 0, 0))
    loom.toggle(w.place(Blocks.Slinger, 18, 18, 0, Valley.Player))
    val plans = Vector(Plan(loom.block, 0, 0, 0, note = loom.note), Plan(Blocks.Belt, 4, -2, 3))
    assert(LayoutCodec.load(LayoutCodec.save(plans)).contains(plans))
    Vector("", "nonsense", "belt 1 2", "hearth 0 0 0 -1 ", "belt 0 0 x -1 ").foreach(t => assert(LayoutCodec.load(t).isEmpty, t))
    // Through a site, into a building.
    val site = w.startSite(loom.block, 30, 30, 0, -1, loom.note)
    site.work(2f)
    w.buildingAt(30, 30) match {
      case l: LoomBuilding => assert(l.cards == loom.cards && l.ties == loom.ties)
      case b => fail(s"$b")
    }
    assert(Card.read("1:2!x!99:0:0:0:0:0:0").isEmpty)
  }

  test("aims are reached one after the other, and rewarded") {
    val rules = quiet.copy(free = false, aims = Vector(
      Aim.Build(Blocks.Wall, 2) -> Vector(Stack(Items.Gear, 7)), Aim.Deliver(Items.Copper, 30) -> Vector.empty, Aim.Reach(1, Season.Summer) -> Vector(Stack(Items.Ember, 3)),
      Aim.Learn(Blocks.Smelter) -> Vector.empty, Aim.Crew(UnitKinds.Beetle, 1) -> Vector.empty, Aim.Send(10) -> Vector.empty, Aim.Wave(3) -> Vector.empty, Aim.Slay(3) -> Vector.empty, Aim.Burn(1) -> Vector.empty
    ), seasons = Vector(20f, 20f, 20f, 20f))
    val w = field(rules)
    w.deliveredCounts(Items.Copper.id) = 12
    run(w, 2f)
    assert(w.aimsReached == 0)
    w.place(Blocks.Wall, 20, 20, 0, Valley.Player)
    w.place(Blocks.Wall, 21, 20, 0, Valley.Player)
    run(w, 1f)
    assert(w.aimsReached == 1 && w.hearthStock(Items.Gear) == 7 && w.aim.get._1.done(w) == 12)
    w.deliveredCounts(Items.Copper.id) = 40
    run(w, 1f)
    assert(w.aimsReached == 2)
    run(w, 24f)
    assert(w.aimsReached >= 3 && w.hearthStock(Items.Ember) == 3)
    assert(Codec.load(Codec.save(w), rules).get.aimsReached == w.aimsReached)
    val out = new Writer
    RulesCodec.write(rules, out)
    assert(RulesCodec.read(new Reader(out.result)) == rules)
    // Those of the campaign can all be reached with what can be learned there.
    Valleys.all.foreach(r => r.aims.foreach { case (a, _) => a match {
      case Aim.Build(b, _) => assert(b.category.nonEmpty && (Pages.byBlock.contains(b)), b.key)
      case Aim.Learn(b) => assert(Pages.byBlock.contains(b), b.key)
      case Aim.Burn(n) => assert(n <= r.nests)
      case Aim.Reach(y, _) => r.goal match { case Goal.Survive(years) => assert(y <= years); case _ => () }
      case _ => ()
    } })
  }

  test("every block can be learned, and what its page asks for can be made by then") {
    Blocks.all.filter(_.category.nonEmpty).foreach(b => assert(Pages.byBlock.contains(b), b.key))
    // No two pages in the same place.
    val places = Pages.all.map(p => (p.column, p.row))
    assert(places.distinct.size == places.size, places.diff(places.distinct))
    Pages.all.foreach(p => p.requires.foreach(r => assert(Pages.byBlock(r).column <= p.column, s"${r.key} before ${p.block.key}")))
  }
}
