package hearthwheel

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import sgl.Input.{Keys, MouseButtons}
import sgl.engine.gameobject.Scene
import hearthwheel.core.{Lang, Overlay, Play, PlayScene, Puppet, Showcase, TitleScene, Tool, ValleyScene}
import hearthwheel.model._

/** Plays the game by its pointer and keys, and looks at what it draws. */
class PlaySuite extends AnyFunSuite with BeforeAndAfterEach {
  private var game: Headless = null
  private val T = Valley.Tile.toFloat
  private val meadow = Rules("Test", "", 64, 5L, Goal.Endless, nests = 1, peaceful = true, free = true, calm = true)

  private def open(scene: Scene, fancy: Boolean = false): Unit = {
    game = new Headless(scene, fancy = fancy)
    game.start()
    // Until the camera has settled on the lamplighter.
    game.step(120)
  }
  override def afterEach(): Unit = if(game != null) { game.stop(); game = null }

  private def field(rules: Rules = meadow): Valley = {
    val w = new Valley(64, 64, rules)
    java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
    w.place(Blocks.Hearth, 30, 30, 0, Valley.Player)
    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, w.hearth.cx, w.hearth.cy + 70f)
    w.learned ++= Blocks.all.filter(_.category.nonEmpty)
    w
  }
  private def play: Play = game.play
  private def world: Valley = play.world
  private def at(tx: Int, ty: Int): (Float, Float) = { val p = play.toScreen((tx + 0.5f) * T, (ty + 0.5f) * T); (p.x, p.y) }
  private def dragTiles(a: (Int, Int), b: (Int, Int), button: MouseButtons.MouseButton = MouseButtons.Left): Unit = {
    val (x0, y0) = at(a._1, a._2)
    val (x1, y1) = at(b._1, b._2)
    game.drag(x0, y0, x1, y1, button)
  }
  private def clickTile(t: (Int, Int), button: MouseButtons.MouseButton = MouseButtons.Left): Unit = { val (x, y) = at(t._1, t._2); game.click(x, y, button) }
  private def block(x: Int, y: Int): String = world.buildingAt(x, y) match { case null => "none"; case b => b.block.key }

  test("a belt is dragged around a corner, and the lamplighter builds it") {
    open(new PlayScene(() => field(), -1))
    play.tool = Tool.Build(Blocks.Belt)
    dragTiles((36, 34), (39, 37))
    val planned = world.builder.plans.map(p => (p.x, p.y)).toSet ++ world.buildings.filter(b => b.isInstanceOf[SiteBuilding] || b.isInstanceOf[BeltBuilding]).map(b => (b.x, b.y))
    assert(planned.size == 7)
    game.step(240)
    val belts = world.buildings.collect { case b: BeltBuilding => b }
    assert(belts.size == 7)
    assert(world.buildingAt(36, 34).rotation == 0 && world.buildingAt(39, 35).rotation == 1)
    assert(world.buildingAt(39, 37).rotation == 1)
  }

  test("the build menu is clicked, a block is placed, then taken down by the right button") {
    open(new PlayScene(() => field(), -1))
    // The menu is in the bottom right corner: defense is the fifth category.
    val s = play.ui.scale
    val h = play.ui.height
    game.click((play.ui.width - 8f - (6 * 38f + 74f) + 22f) * s, (h - 8f - (4 * 38f + 64f) + 8f + 4 * 28.6f + 12f) * s)
    game.step(2)
    assert(play.category == Category.Guard)
    play.tool = Tool.Build(Blocks.Boltcaster)
    clickTile((38, 32))
    game.step(400)
    assert(block(38, 32) == "boltcaster")
    game.key(Keys.Escape)
    assert(play.tool == Tool.Hand)
    dragTiles((37, 31), (40, 34), MouseButtons.Right)
    game.step(400)
    assert(block(38, 32) == "none")
  }

  test("the pointer never builds through the interface") {
    open(new PlayScene(() => field(), -1))
    play.tool = Tool.Build(Blocks.Wall)
    game.click(1200f, 80f)
    game.click(60f, 60f)
    game.click(1150f, 600f)
    assert(world.builder.plans.isEmpty && world.buildings.size == 1)
  }

  test("a layout is copied, turned and stamped") {
    open(new PlayScene(() => {
      val w = field()
      w.place(Blocks.Belt, 35, 35, 0, Valley.Player)
      w.place(Blocks.Sorter, 36, 35, 0, Valley.Player, config = Items.Tin.id)
      w
    }, -1))
    game.key(Keys.C)
    dragTiles((34, 34), (37, 36))
    play.tool match {
      case Tool.Stamp(plans) => assert(plans.size == 2)
      case other => fail(other.toString)
    }
    game.key(Keys.R)
    clickTile((32, 37))
    game.step(200)
    val copies = world.buildings.filter(b => b.block eq Blocks.Sorter)
    assert(copies.size == 2 && copies.forall(_.config == Items.Tin.id))
    val belts = world.buildings.collect { case b: BeltBuilding => b }
    assert(belts.map(_.rotation).toSet == Set(0, 1))
  }

  test("ziplines chain by dragging, and pylons link by tapping") {
    open(new PlayScene(() => field(), -1))
    play.tool = Tool.Build(Blocks.Zipline)
    dragTiles((24, 35), (34, 35))
    game.step(400)
    val lines = world.buildings.collect { case z: ZiplineBuilding => z }.sortBy(_.x)
    assert(lines.map(_.x).toVector == Vector(24, 28, 32, 34))
    assert(lines.init.zip(lines.tail).forall { case (a, b) => a.destination eq b })
    game.key(Keys.Escape)
    val pylon = world.place(Blocks.Pylon, 30, 26, 0, Valley.Player)
    val mill = world.place(Blocks.Windmill, 33, 26, 0, Valley.Player)
    game.step(5)
    // What is put up in reach is linked by itself; tapping lets go, and links again.
    assert(mill.links.contains(pylon))
    clickTile((30, 26))
    assert(play.selected eq pylon)
    clickTile((33, 26))
    assert(mill.links.isEmpty)
    clickTile((33, 26))
    assert(mill.links.contains(pylon))
  }

  test("keys pause, hurry, and open and close what they should") {
    open(new PlayScene(() => field(), -1))
    val before = world.time
    game.key(Keys.Space)
    assert(play.paused)
    val paused = world.time
    game.step(30)
    assert(world.time == paused && paused >= before)
    game.key(Keys.Space)
    game.key(Keys.V)
    val t = world.time
    game.step(30)
    assert(world.time - t > 0.9f)
    game.key(Keys.T)
    assert(play.overlay == Overlay.Learn)
    game.step(3)
    game.key(Keys.Escape)
    assert(play.overlay == Overlay.None)
    game.key(Keys.Escape)
    assert(play.overlay == Overlay.Menu)
    game.step(3)
    game.key(Keys.Escape)
    assert(play.overlay == Overlay.None)
  }

  test("the lamplighter flies, shoots what is pointed at, and comes back after its light goes out") {
    open(new PlayScene(() => field(), -1))
    val x = world.lantern.x
    game.controllerDown(Keys.D)
    game.step(40)
    game.controllerUp(Keys.D)
    assert(world.lantern.x > x + 60f)
    game.step(60)
    val foe = world.spawn(UnitKinds.Stumper, Valley.Bramble, world.lantern.x + 120f, world.lantern.y)
    foe.frozen = 100f
    val p = play.toScreen(foe.x, foe.y)
    game.hold(p.x, p.y, 90)
    assert(foe.hp < foe.maxHp)
    foe.hp = 0f
    world.killCreature(foe)
    world.killCreature(world.lantern)
    game.step(300)
    assert(world.lantern != null && world.lantern.alive)
  }

  test("automata are boxed and sent") {
    open(new PlayScene(() => {
      val w = field()
      w.spawn(UnitKinds.Beetle, Valley.Player, 34 * T, 36 * T)
      w.spawn(UnitKinds.Kite, Valley.Player, 35 * T, 36 * T)
      w
    }, -1))
    game.key(Keys.F)
    dragTiles((32, 34), (37, 38))
    val crew = world.creatures.filter(c => c.kind.friendly && !(c.kind eq UnitKinds.Lantern))
    assert(crew.size == 2 && crew.forall(_.selected))
    val (x, y) = at(26, 33)
    game.click(x, y, MouseButtons.Right)
    game.step(700)
    crew.foreach(c => assert(MathUtil.dist(c.x, c.y, 26.5f * T, 33.5f * T) < 80f, c.kind.key))
  }

  test("every creature has a game object while it lives, and none after") {
    open(new PlayScene(() => field(), -1))
    for(i <- 0 until 40) { val _ = world.spawn(UnitKinds.Burr, Valley.Bramble, 200f + i * 12f, 300f) }
    game.step(3)
    assert(game.world.index[Puppet].size == 41)
    world.creatures.filter(_.team == Valley.Bramble).foreach(world.killCreature)
    game.step(3)
    assert(game.world.index[Puppet].size == 1)
  }

  test("a valley is saved on leaving, and goes on as it was") {
    open(new PlayScene(() => MapGen.generate(Valleys.all(0)), 0))
    play.world.place(Blocks.Wall, world.hearth.x + 6, world.hearth.y, 0, Valley.Player)
    val (hx, hy) = (world.hearth.x, world.hearth.y)
    game.step(120)
    val time = world.time
    game.load(ValleyScene)
    game.step(5)
    // The first valley's panel offers to go on with it.
    val scale = 1280f / 1180f min 720f / 680f
    val w = 1280f / scale
    game.click((w - 440f + 14f + 24f + 60f) * scale, (720f / scale - 20f - 60f + 22f) * scale)
    game.step(10)
    assert(game.inValley)
    assert(world.time >= time - 0.01f && world.time < time + 2f)
    assert(block(hx + 6, hy) == "wall")
  }

  test("losing shows the end, and the valley can be started over") {
    open(new PlayScene(() => field(), -1))
    world.hearth.damage(1e9f)
    game.step(10)
    assert(world.outcome == Outcome.Lost && play.overlay == Overlay.End)
    val s = play.ui.scale
    game.click(640f - 90f * s, 360f + 120f * s)
    game.step(10)
    assert(world.outcome == Outcome.Playing && play.overlay == Overlay.None)
  }

  test("winning a valley opens the next one and keeps what was learned") {
    open(new PlayScene(() => MapGen.generate(Valleys.all(0)), 0))
    world.learned += Blocks.Smelter
    world.hearth.stock.add(Items.Copper, 10)
    world.year = 1
    world.seasonIndex = 3
    world.seasonTime = world.seasonLength - 0.05f
    game.step(20)
    assert(world.outcome == Outcome.Won)
    val profile = game.world.globals.get[core.Profile]
    assert(profile.unlocked(1) && profile.learned.contains(Blocks.Smelter))
  }

  test("every season, every screen and a busy fight draw without a hitch") {
    open(TitleScene, fancy = true)
    game.step(30)
    game.load(ValleyScene)
    game.step(10)
    Season.values.foreach { s =>
      game.load(new PlayScene(() => Showcase.build(MapGen.generate(Valleys.all(2).copy(startSeason = s.ordinal, seasons = Vector(6f, 6f, 6f, 6f), free = true))), -1))
      game.step(3)
      for(k <- UnitKinds.all; i <- 0 until 4 if !(k eq UnitKinds.Lantern)) {
        val a = i * 1.57f + k.id
        val _ = world.spawn(k, if(k.friendly) Valley.Player else Valley.Bramble, world.hearth.cx + MathUtil.cos(a) * 200f, world.hearth.cy + MathUtil.sin(a) * 200f)
      }
      play.zoomGoal = 1f
      game.step(90)
      val frame = game.frame()
      // Something of everything is there: the picture is far from one color.
      val colors = scala.collection.mutable.HashSet.empty[Int]
      for(i <- 0 until frame.width * frame.height by 97) colors += (frame.rgba(i * 4) & 0xff) << 16 | (frame.rgba(i * 4 + 1) & 0xff) << 8 | (frame.rgba(i * 4 + 2) & 0xff)
      assert(colors.size > 500, s.label)
    }
  }

  test("belts cross other belts by themselves, go over walls on ziplines, and find their way around") {
    open(new PlayScene(() => {
      val w = field()
      for(y <- 33 to 39) { val _ = w.place(Blocks.Belt, 34, y, 1, Valley.Player) }
      w.place(Blocks.WallLarge, 37, 35, 0, Valley.Player)
      w.place(Blocks.WallLarge, 37, 37, 0, Valley.Player)
      w
    }, -1))
    play.tool = Tool.Build(Blocks.Belt)
    dragTiles((31, 36), (41, 36))
    game.step(300)
    assert(block(34, 36) == "crossing" && block(36, 36) == "zipline" && block(39, 36) == "zipline" && block(41, 36) == "belt")
    game.controllerDown(Keys.LeftShift)
    dragTiles((36, 38), (39, 38))
    game.controllerUp(Keys.LeftShift)
    game.step(300)
    assert(world.buildings.count { case b: BeltBuilding => b.y >= 39 || b.y <= 34; case _ => false } > 4)
    assert(block(37, 37) == "wall_large")
  }

  test("a seam is dug by hand, and what is carried is handed to a turret") {
    open(new PlayScene(() => {
      val w = field(meadow.copy(free = false))
      w.learned ++= Blocks.all.filter(_.category.nonEmpty)
      w.ores(35 * 64 + 33) = Items.Copper.id.toByte
      w.place(Blocks.Slinger, 35, 34, 0, Valley.Player)
      w
    }, -1))
    clickTile((33, 35))
    assert(world.mining == 35 * 64 + 33)
    game.step(200)
    clickTile((33, 35))
    game.step(3)
    assert(world.mining == -1 && world.hearthStock(Items.Copper) > 0)
    world.cargo = Items.Flint
    world.cargoAmount = 20
    clickTile((35, 34))
    assert(world.cargoAmount < 20)
  }

  test("an automaton is ridden and gets off again, and groups are kept") {
    open(new PlayScene(() => {
      val w = field()
      w.spawn(UnitKinds.Stag, Valley.Player, 34 * T, 36 * T)
      w.spawn(UnitKinds.Kite, Valley.Player, 37 * T, 36 * T)
      w
    }, -1))
    val stag = world.creatures.find(_.kind eq UnitKinds.Stag).get
    val p = play.toScreen(stag.x, stag.y)
    game.move(p.x, p.y)
    game.key(Keys.E)
    assert(play.riding eq stag)
    val x = stag.x
    game.controllerDown(Keys.D)
    game.step(60)
    game.controllerUp(Keys.D)
    assert(stag.x > x + 20f)
    game.key(Keys.E)
    assert(play.riding == null && !stag.controlled)
    game.key(Keys.Y)
    val crew = world.creatures.filter(c => c.team == Valley.Player && !UnitKinds.isLantern(c.kind))
    assert(crew.forall(_.selected))
    game.controllerDown(Keys.LeftShift)
    game.key(Keys.Num3)
    game.controllerUp(Keys.LeftShift)
    crew.foreach(_.selected = false)
    game.key(Keys.Num3)
    assert(crew.forall(_.selected))
  }

  test("R turns a belt where it stands, and Z flips a layout") {
    open(new PlayScene(() => {
      val w = field()
      w.place(Blocks.Belt, 34, 35, 0, Valley.Player)
      w
    }, -1))
    val (x, y) = at(34, 35)
    game.move(x, y)
    game.key(Keys.R)
    assert(world.buildingAt(34, 35).rotation == 1)
    play.tool = Tool.Stamp(Vector(Plan(Blocks.Belt, 2, 0, 0), Plan(Blocks.Drill, -3, 1, 0)))
    game.key(Keys.Z)
    play.tool match {
      case Tool.Stamp(plans) => assert(plans == Vector(Plan(Blocks.Belt, -3, 0, 2), Plan(Blocks.Drill, 1, 1, 0)))
      case other => fail(other.toString)
    }
  }

  test("keys can be changed, and the change is kept") {
    val b = new core.Bindings
    b.bind(core.Command.Pause, Keys.P)
    assert(b.keyOf(core.Command.Pause) == Keys.P)
    b.bind(core.Command.Speed, Keys.P)
    assert(b.keyOf(core.Command.Speed) == Keys.P && b.keyOf(core.Command.Pause) == Keys.V)
    val again = new core.Bindings
    again.load(b.save)
    assert(core.Command.values.forall(c => again.keyOf(c) == b.keyOf(c)))
    assert(core.Command.values.map(_.default).distinct.length == core.Command.values.length)
  }

  test("every overlay draws, and leaves the valley as it was") {
    open(new PlayScene(() => Showcase.build(MapGen.generate(Valleys.all(1).copy(free = true, calm = true))), -1))
    game.step(60)
    val buildings = world.buildings.size
    Overlay.values.filterNot(o => o == Overlay.End || o == Overlay.None || o == Overlay.Loom).foreach { o =>
      play.overlay = o
      game.step(3)
      game.move(400f, 300f)
      game.step(2)
      assert(play.overlay == o, o.toString)
    }
    play.overlay = Overlay.None
    // One of everything, with its panel open.
    Blocks.all.filter(b => b.category.nonEmpty && !(b.role eq Role.Hearth)).foreach { b =>
      play.selected = world.make(b, 5, 5, 0, Valley.Player)
      game.step()
    }
    play.selected = world.hearth
    game.step(2)
    assert(world.buildings.size == buildings)
  }

  test("games are saved and loaded by hand") {
    open(new PlayScene(() => MapGen.generate(Valleys.all(1)), 1))
    val profile = game.world.globals.get[core.Profile]
    world.hearth.stock.add(Items.Gear, 123)
    profile.keepGame(world, 1)
    assert(profile.games.size == 1 && profile.games.head.name == "Copperbrook")
    val text = profile.gameText(profile.games.head).get
    assert(Codec.load(text, profile.games.head.rules).get.hearthStock(Items.Gear) == 123)
    // And found again by another profile.
    val again = new core.Profile(game.world.globals.get[core.Services])
    assert(again.games.map(_.rules) == profile.games.map(_.rules.copy(about = "")))
    profile.dropGame(profile.games.head)
    assert(profile.games.isEmpty)
  }

  test("what wagons take along is brought to the next valley") {
    open(new PlayScene(() => MapGen.generate(Valleys.all(0)), 0))
    world.sentOut(Items.Bronze.id) = 200
    val profile = game.world.globals.get[core.Profile]
    game.load(ValleyScene)
    game.step(5)
    assert(profile.vault(Items.Bronze.id) == 200)
  }

  test("a map is painted, kept and played") {
    open(new core.EditorScene(new HandMap(64), 1))
    game.drag(600f, 300f, 760f, 320f)
    game.step(5)
    val profile = game.world.globals.get[core.Profile]
    val scale = 1280f / 1180f min 720f / 680f
    game.click(90f * scale, (720f / scale - 112f + 15f) * scale)
    game.step(5)
    assert(profile.maps == Vector(1))
    val m = profile.mapText(1).get
    assert(m.floors.count(_ == Floor.Loam.ordinal) > 10)
    m.nests += ((4, 4))
    m.ores(30 * 64 + 20) = Items.Iron.id.toByte
    val w = m.build(core.EditorScene.rules(1, m))
    assert(w.nests.size == 1 && w.oreAt(20, 30) == Items.Iron.id && w.hearth != null)
    game.load(core.EditorMenuScene)
    game.step(5)
  }

  test("deeds are noticed, and hints given once") {
    open(new PlayScene(() => MapGen.generate(Valleys.all(0)), 0))
    world.deliveredCounts(Items.Copper.id) = 5
    game.step(140)
    val profile = game.world.globals.get[core.Profile]
    assert(profile.deeds.contains("first_copper"))
  }

  test("layouts, maps, cards and games go over the clipboard") {
    open(new PlayScene(() => field(), -1))
    val services = game.world.globals.get[core.Services]
    val profile = game.world.globals.get[core.Profile]
    play.tool = Tool.Stamp(Vector(Plan(Blocks.Belt, 0, 0, 1), Plan(Blocks.Slinger, 1, 0, 0)))
    play.overlay = Overlay.Layouts
    game.step(2)
    val scale = scala.math.min(1280f / 1180f, 720f / 680f)
    val (x, y) = (1280f / scale / 2f - 410f, 720f / scale / 2f - 270f)
    def press(px: Float, py: Float): Unit = game.click(px * scale, py * scale)
    press(x + 240f, y + 28f)
    assert(profile.layouts.size == 1)
    press(x + 26f + 184f - 56f, y + 60f + 123f)
    var text = ""
    services.paste(t => text = t)
    assert(text.startsWith("hearthwheel-layout/"))
    press(x + 820f - 170f, y + 28f)
    assert(profile.layouts.size == 2 && profile.layouts(0) == profile.layouts(1))
    services.copy("something else")
    press(x + 820f - 170f, y + 28f)
    assert(profile.layouts.size == 2)
  }

  test("a loom is linked by tapping, and its cards are punched") {
    open(new PlayScene(() => {
      val w = field()
      w.place(Blocks.Loom, 34, 34, 0, Valley.Player)
      w.place(Blocks.Slinger, 38, 34, 0, Valley.Player)
      w
    }, -1))
    clickTile((34, 34))
    clickTile((38, 34))
    val loom = world.buildings.collectFirst { case l: LoomBuilding => l }.get
    assert(loom.tied(0) != null && (loom.tied(0).block eq Blocks.Slinger))
    clickTile((34, 34))
    assert(play.overlay == Overlay.Loom)
    game.step(2)
    val scale = scala.math.min(1280f / 1180f, 720f / 680f)
    val (x, y) = (1280f / scale / 2f - 540f, 720f / scale / 2f - 300f)
    game.click((x + 170f) * scale, (y + 28f) * scale)
    assert(loom.cards == Vector(Card.blank))
    // The right half of what it looks at goes on to the next.
    game.click((x + 20f + 46f + 150f) * scale, (y + 58f + 24f) * scale)
    assert(loom.cards.head.sense == Sense.Hearth)
    game.key(Keys.Escape)
    assert(play.overlay == Overlay.None)
  }

  test("boats are sent across the water, and drawn with a wake") {
    open(new PlayScene(() => {
      val w = field()
      for(y <- 24 to 38; x <- 20 to 29) w.floors(y * 64 + x) = Floor.Deep.ordinal.toByte
      w.spawn(UnitKinds.Skiff, Valley.Player, 22f * T, 28.5f * T)
      w
    }, -1))
    val skiff = world.creatures.find(_.kind eq UnitKinds.Skiff).get
    play.tool = Tool.Command
    skiff.selected = true
    clickTile((28, 28), MouseButtons.Right)
    game.step(120)
    assert(skiff.x > 24f * T, skiff.x / T)
  }

  private def inFrench(body: => Unit): Vector[String] = {
    Lang.language = Lang.Language.French
    Lang.missing = scala.collection.mutable.LinkedHashSet.empty
    try { body; Lang.missing.toVector }
    finally { Lang.missing = null; Lang.language = Lang.Language.English }
  }

  test("all there is has a name and a description in French") {
    assert(inFrench {
      Blocks.all.foreach { b => Lang(b.label); Lang(b.about); core.Facts.of(b).foreach(Lang(_)) }
      Items.all.foreach { i => Lang(i.label); Lang(i.about) }
      UnitKinds.all.foreach { k => Lang(k.label); Lang(k.about) }
      Season.values.foreach { x => Lang(x.label); Lang(x.hint) }
      core.Command.values.foreach(c => Lang(c.label))
      core.Deeds.all.foreach { d => Lang(d.label); Lang(d.text) }
      Valleys.all.foreach { r => Lang(r.name); Lang(r.about); Lang(r.goal.label); r.aims.foreach(a => Lang(a._1.label)) }
      Sense.values.foreach(x => Lang(x.label)); Doing.values.foreach(x => Lang(x.label)); Watch.values.foreach(x => Lang(x.label))
      SkyEvent.values.foreach(x => Lang(x.label))
    } == Vector.empty)
  }

  test("what changes within a text is translated too") {
    val _ = inFrench {
      assert(Lang("Bring 60 Bronze to the Hearth") == "Apporter 60 Bronze au Foyer")
      assert(Lang("Build a Firebox") == "Construire : Chaudière simple")
      assert(Lang("Summer in 2:41") == "Été dans 2:41")
      assert(Lang("See the winter of year 2") == "Voir venir : hiver, année 2")
      assert(Lang("2 Copper + 1 Tin = 2 Bronze, 1.5s") == "2 Cuivre + 1 Étain = 2 Bronze, 1.5 s", Lang("2 Copper + 1 Tin = 2 Bronze, 1.5s"))
      assert(Lang("Wave 12") == "Vague 12" && Lang("3:20") == "3:20" && Lang("") == "")
      // What nobody has translated is left alone.
      assert(Lang("zzz qqq") == "zzz qqq")
    }
    assert(Lang("Wave 12") == "Wave 12")
  }

  test("every screen is in French when it is asked for") {
    val left = inFrench {
      open(new PlayScene(() => Showcase.build(MapGen.generate(Valleys.all(1).copy(free = true, calm = true))), 1))
      Overlay.values.filterNot(o => o == Overlay.Loom).foreach { o => play.overlay = o; game.step(2); game.move(420f, 330f); game.step(2) }
      play.overlay = Overlay.None
      Blocks.all.filter(b => b.category.nonEmpty && !(b.role eq Role.Hearth)).foreach { b => play.selected = world.make(b, 5, 5, 0, Valley.Player); game.step() }
      play.selected = world.hearth
      game.step()
      val loom = world.place(Blocks.Loom, 5, 5, 0, Valley.Player).asInstanceOf[LoomBuilding]
      loom.cards = Sense.values.toVector.take(8).map(x => Card(x, 0, 1, 1, Doing.Stop, 0, 0))
      play.selected = loom
      play.overlay = Overlay.Loom
      game.step(2)
      play.overlay = Overlay.None
      play.selected = null
      Vector(Tool.Build(Blocks.Belt), Tool.Build(Blocks.Drill), Tool.Remove, Tool.Copy, Tool.Command, Tool.Rebuild, Tool.Stamp(Vector.empty)).foreach { t => play.tool = t; game.step(2) }
      game.load(ValleyScene); game.step(4)
      game.click(1030f, 40f); game.step(4)
      game.load(core.EditorMenuScene); game.step(4)
      game.load(TitleScene); game.step(4)
    }
    assert(left == Vector.empty)
  }

  test("a map is named, given rules and aims, and keeps them") {
    val map = new HandMap(64)
    open(new core.EditorScene(map, 1))
    val scale = scala.math.min(1280f / 1180f, 720f / 680f)
    val (pw, ph) = (1280f / scale, 720f / scale)
    game.click(90f * scale, (ph - 130f) * scale)
    game.step(3)
    val (x, y) = (pw / 2f - 530f, ph / 2f - 310f)
    game.type_("My valley")
    game.key(Keys.Backspace)
    game.step(2)
    assert(map.rules.name == "My valle")
    // The goal, then an aim.
    game.click((x + 100f + 2 * 92f + 40f) * scale, (y + 115f) * scale)
    assert(map.rules.goal == Goal.Survive(2))
    game.click((x + 150f) * scale, (y + 298f + 8f) * scale)
    assert(map.rules.aims.map(_._1) == Vector(Aim.Wave(5)))
    // On to the next kind of aim.
    game.click((x + 46f + 110f) * scale, (y + 298f + 32f + 18f) * scale)
    assert(map.rules.aims.head._1 == Aim.Slay(50))
    map.nests += ((4, 4))
    val back = HandMap.load(map.save).get
    assert(back.rules == map.rules)
    assert(core.EditorScene.rules(3, back).name == "My valle (3)")
    // What was made before there were rules is still read.
    val old = { val t = map.save.split(' '); ("1" +: t.drop(1)).mkString(" ") }
    assert(HandMap.load(old).get.rules == HandMap.usual)
  }
}
