package hearthwheel

import java.nio.file.Paths

import hearthwheel.core.{Overlay, PlayScene, Showcase, TitleScene, Tool, ValleyScene}
import hearthwheel.model._

/** Saves pictures of the game: `capture <directory>`. */
object Capture {
  def main(args: Array[String]): Unit = {
    val out = Paths.get(args(0))
    val only = args.drop(1).toSet
    val game = new Headless(TitleScene)
    game.start()
    def shot(name: String)(prepare: => Unit): Unit = if(only.isEmpty || only.contains(name)) {
      prepare
      game.save(out, name)
      println(name)
    }
    shot("title") { game.step(200) }
    shot("valleys") { game.load(ValleyScene); game.step(6) }
    shot("own") { game.load(ValleyScene); game.step(3); game.click(1030f, 40f); game.step(30) }
    shot("start") { game.load(new PlayScene(() => MapGen.generate(Valleys.all(0)), 0)); game.step(60) }
    shot("learn") { game.load(new PlayScene(() => MapGen.generate(Valleys.all(0)), 0)); game.step(10); game.play.overlay = Overlay.Learn; game.step(3); game.move(880f, 375f); game.step(2) }
    Season.values.foreach { s =>
      shot(s.label.toLowerCase) {
        val rules = Valleys.all(1).copy(startSeason = s.ordinal, free = true, firstWave = 3f, threat = 3f)
        game.load(new PlayScene(() => Showcase.build(MapGen.generate(rules)), -1))
        game.step(3)
        game.play.zoomGoal = 1.5f
        game.step(1500)
        game.play.messages.clear()
        game.step(2)
      }
    }
    shot("close") {
      game.load(new PlayScene(() => Showcase.build(MapGen.generate(Valleys.all(1).copy(free = true))), -1))
      game.step(3)
      game.play.zoomGoal = 2.8f
      game.step(400)
      game.play.selected = game.play.world.buildings.find(_.isInstanceOf[CrafterBuilding]).orNull
      game.step(3)
    }
    shot("placing") {
      game.load(new PlayScene(() => MapGen.generate(Valleys.all(0).copy(free = true)), -1))
      game.step(60)
      game.play.tool = Tool.Build(Blocks.Boltcaster)
      game.move(760f, 300f)
      game.step(4)
    }
    shot("far") {
      game.load(new PlayScene(() => Showcase.build(MapGen.generate(Valleys.all(3).copy(free = true))), -1))
      game.step(3)
      game.play.zoomGoal = 0.1f
      game.step(120)
    }
    def field(season: Int): Valley = {
      val w = MapGen.generate(Valleys.all(2).copy(free = true, startSeason = season, seasons = Vector(900f, 900f, 900f, 900f), firstWave = 900f, goal = Goal.Wheel))
      Showcase.build(w)
    }
    def horde(kinds: Seq[UnitKind], n: Int): Unit = {
      val w = game.play.world
      for(k <- kinds; i <- 0 until n) {
        val a = -0.6f + 1.2f * i / n + k.id * 0.2f
        val _ = w.spawn(k, if(k.friendly) Valley.Player else Valley.Bramble, w.hearth.cx + MathUtil.cos(a) * (300f + k.id * 9f), w.hearth.cy + MathUtil.sin(a) * (300f + k.id * 9f))
      }
    }
    shot("fight") {
      game.load(new PlayScene(() => field(1), -1)); game.step(3); game.play.zoomGoal = 1.7f
      horde(Seq(UnitKinds.Burr, UnitKinds.Nettle, UnitKinds.Stumper, UnitKinds.Dandel, UnitKinds.Hornwing, UnitKinds.Creepvine), 5)
      game.step(170)
    }
    shot("boss") {
      game.load(new PlayScene(() => field(3), -1)); game.step(3); game.play.zoomGoal = 1.7f
      horde(Seq(UnitKinds.Gnarl, UnitKinds.Rimewood), 1)
      horde(Seq(UnitKinds.Podder), 2)
      game.step(200)
    }
    shot("crew") {
      game.load(new PlayScene(() => field(0), -1)); game.step(3); game.play.zoomGoal = 2.2f
      horde(Seq(UnitKinds.Beetle, UnitKinds.Kite, UnitKinds.Golem, UnitKinds.Lanternfly), 2)
      game.play.world.creatures.foreach(c => if(c.kind.friendly) { c.selected = true; c.x -= 120f; c.rallyX = c.x; c.rallyY = c.y })
      game.play.tool = Tool.Command
      game.step(40)
    }
    shot("wheel") {
      game.load(new PlayScene(() => {
        val w = new Valley(64, 64, Rules("Show", "", 64, 5L, Goal.Wheel, nests = 0, free = true, peaceful = true))
        java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
        w.place(Blocks.Hearth, 30, 30, 0, Valley.Player)
        w.learned ++= Blocks.all
        w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, w.hearth.cx, w.hearth.cy + 70f)
        for(x <- 20 to 27; y <- 34 to 37) w.floors(y * 64 + x) = (if(x < 22) Floor.Deep else Floor.Shallow).ordinal.toByte
        val g = w.place(Blocks.GreatWheel, 34, 28, 0, Valley.Player).asInstanceOf[GreatWheelBuilding]
        for(s <- 0 until 2) Blocks.spokes(s).foreach(k => g.filledBy(s).add(k.item, k.amount))
        Blocks.spokes(2).foreach(k => g.filledBy(2).add(k.item, k.amount / 2))
        var x = 18
        for(b <- Vector(Blocks.Auger, Blocks.EmberForge, Blocks.Toyworks, Blocks.Foundry, Blocks.Lance)) { w.place(b, x, 23, 0, Valley.Player); x += b.size + 1 }
        x = 18
        for(b <- Vector(Blocks.Waterwheel, Blocks.SunLens, Blocks.Firebox, Blocks.TallPylon, Blocks.Radiator, Blocks.Storehouse, Blocks.WallLarge, Blocks.BronzeWallLarge)) { w.place(b, x, 28, 0, Valley.Player); x += b.size; }
        x = 18
        for(b <- Vector(Blocks.Pump, Blocks.Mister, Blocks.Tap, Blocks.Sorter, Blocks.Spillway, Blocks.Hatch, Blocks.Crossing, Blocks.Splitter, Blocks.BronzeWall)) { w.place(b, x, 32, 0, Valley.Player); x += 1 }
        for(i <- 18 to 24) { val _ = w.place(Blocks.Pipe, i, 33, 0, Valley.Player) }
        for(i <- 34 to 36) { val _ = w.place(Blocks.Pipe, 24, i, 0, Valley.Player) }
        w
      }, -1)); game.step(3); game.play.zoomGoal = 1.9f
      game.play.selected = game.play.world.buildings.find(_.isInstanceOf[GreatWheelBuilding]).get
      game.step(90)
    }
    shot("zoo") {
      game.load(new PlayScene(() => {
        val w = new Valley(64, 64, Rules("Show", "", 64, 5L, Goal.Endless, nests = 0, free = true, peaceful = true))
        java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
        w.place(Blocks.Hearth, 40, 24, 0, Valley.Player)
        w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, 29 * 32f, 30 * 32f)
        w
      }, -1)); game.step(3); game.play.zoomGoal = 2.6f
      UnitKinds.all.filterNot(_ eq UnitKinds.Lantern).zipWithIndex.foreach { case (k, i) =>
        // All on one side, so that they stand and look.
        val c = game.play.world.spawn(k, Valley.Player, (24.5f + (i % 5) * 2.4f) * 32, (27.2f + (i / 5) * 2.6f) * 32)
        c.rallyX = c.x; c.rallyY = c.y
      }
      game.step(60)
    }
    def everything(season: Int): Valley = {
      val w = new Valley(64, 64, Rules("Show", "", 64, 5L, Goal.Endless, nests = 0, free = true, peaceful = true, calm = true, startSeason = season))
      java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
      w.place(Blocks.Hearth, 55, 55, 0, Valley.Player)
      w.learned ++= Blocks.all
      w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, 22f * 32, 34f * 32)
      var x = 12
      var y = 24
      var tall = 0
      Blocks.all.filter(b => b.category.nonEmpty && !b.isBelt && !(b.role eq Role.Hearth) && b.size < 5).sortBy(-_.size).foreach { b =>
        if(x + b.size > 46) { x = 12; y += tall + 1; tall = 0 }
        w.place(b, x, y, 0, Valley.Player) match {
          case c: CrafterBuilding => c.recipe.in.foreach(s => c.stock.add(s.item, 500)); c.activity = 1f
          case r: ReactorBuilding => for(_ <- 0 until 20) r.receive(Items.Ember, null); r.heat = 0.3f
          case g: GeneratorBuilding if g.stock != null => for(_ <- 0 until 8) g.receive(Items.Ember, null)
          case t: TurretBuilding => t.role.ammo.foreach { case (i, _) => for(_ <- 0 until 4) t.receive(i, null) }
          case _ => ()
        }
        tall = scala.math.max(tall, b.size)
        x += b.size + 1
      }
      for(k <- 0 until 3) { val _ = w.place(Blocks.TallPylon, 16 + k * 10, 22, 0, Valley.Player) }
      for(k <- 0 until 4) { val _ = w.place(Blocks.SunCourt, 14 + k * 4, 18, 0, Valley.Player) }
      w
    }
    shot("blocks") { game.load(new PlayScene(() => everything(1), -1)); game.step(3); game.play.zoomGoal = 1.15f; game.play.following = false; game.play.centerX = 29f * 32; game.play.centerY = 29f * 32; game.play.profile.signs = false; game.step(160); game.play.profile.signs = true }
    shot("grand") {
      game.load(new PlayScene(() => { val w = everything(3); w.place(Blocks.GrandHearth, 20, 48, 0, Valley.Player); w.place(Blocks.StarForge, 27, 48, 0, Valley.Player); w.place(Blocks.GreatHearth, 34, 48, 0, Valley.Player)
        w.spawn(UnitKinds.Colossus, Valley.Player, 18f * 32, 46f * 32); w.spawn(UnitKinds.Skybarge, Valley.Player, 24f * 32, 45f * 32); w.spawn(UnitKinds.Heron, Valley.Player, 30f * 32, 46f * 32); w.spawn(UnitKinds.Queen, Valley.Player, 36f * 32, 46f * 32); w.creatures.foreach { c => c.rallyX = c.x; c.rallyY = c.y }; w }, -1))
      game.step(3); game.play.zoomGoal = 1.7f; game.play.following = false; game.play.centerX = 27f * 32; game.play.centerY = 49f * 32; game.step(120)
    }
    shot("codex") { game.load(new PlayScene(() => everything(0), -1)); game.step(3); game.play.codexBlock = Blocks.Skyrack; game.play.overlay = Overlay.Codex; game.step(3) }
    shot("codex3") { game.load(new PlayScene(() => everything(0), -1)); game.step(3); game.play.overlay = Overlay.Codex; game.step(3); game.click(460f, 70f); game.move(150f, 180f); game.step(3) }
    shot("waves") { game.load(new PlayScene(() => MapGen.generate(Valleys.all(1)), 1)); game.step(3); game.play.world.waves.number = 17; game.play.overlay = Overlay.Waves; game.step(3) }
    shot("map") { game.load(new PlayScene(() => field(0), -1)); game.step(3); game.play.overlay = Overlay.Map; game.step(3) }
    shot("ledger") { game.load(new PlayScene(() => field(0), -1)); game.step(3); game.play.speed = 4; game.step(700); game.play.overlay = Overlay.Ledger; game.step(3) }
    shot("deeds") { game.load(new PlayScene(() => field(0), -1)); game.step(90); game.play.overlay = Overlay.Deeds; game.step(3) }
    shot("settings") { game.load(new PlayScene(() => field(0), -1)); game.step(3); game.play.overlay = Overlay.Settings; game.step(3) }
    shot("keys") { game.load(new PlayScene(() => field(0), -1)); game.step(3); game.play.overlay = Overlay.Keys; game.step(3) }
    shot("editor") { game.load(new hearthwheel.core.EditorScene(HandMap.from(MapGen.generate(Valleys.all(3))), 1)); game.step(8) }
    shot("governor") {
      game.load(new PlayScene(() => everything(0), -1)); game.step(3)
      game.play.selected = game.play.world.buildings.find(_.isInstanceOf[GovernorBuilding]).get; game.step(4)
    }
    shot("fire") {
      game.load(new PlayScene(() => field(1), -1)); game.step(3); game.play.zoomGoal = 1.9f
      val h = game.play.world.hearth
      for(i <- 0 until 14) game.play.world.fires.start(h.x + 4 + i % 5, h.y + i / 5)
      for(i <- 0 until 6) game.play.world.explode(h.cx - 150f + i * 22f, h.cy + 90f + (i % 3) * 20f, 34f, 0f, 1, 0f, 0f, true, true, 0xff8a3c)
      game.step(70)
    }
    shot("boats") {
      game.load(new PlayScene(() => {
        val w = everything(0)
        for(y <- 40 to 60; x <- 4 to 40) w.floors(y * 64 + x) = (if(x < 32 && y > 42) Floor.Deep else Floor.Shallow).ordinal.toByte
        w.place(Blocks.Slipway, 8, 46, 0, Valley.Player); w.place(Blocks.Drydock, 12, 46, 0, Valley.Player)
        for((k, i) <- Vector(UnitKinds.Skiff, UnitKinds.Cutter, UnitKinds.Icebreaker, UnitKinds.Skiff).zipWithIndex) { val c = w.spawn(k, Valley.Player, (8f + i * 2f) * 32, (52f + i * 1.6f) * 32); c.rallyX = c.x + 400f; c.rallyY = c.y }
        w
      }, -1)); game.step(3); game.play.zoomGoal = 1.8f; game.play.following = false; game.play.centerX = 15f * 32; game.play.centerY = 52f * 32; game.step(80)
    }
    shot("roots") {
      game.load(new PlayScene(() => {
        val w = new Valley(64, 64, Rules("Show", "", 64, 5L, Goal.Endless, nests = 1, free = true, timer = false, calm = true))
        java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
        w.place(Blocks.Hearth, 8, 30, 0, Valley.Player)
        w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, 24f * 32, 24f * 32)
        w.place(Blocks.Nest, 38, 30, 0, Valley.Bramble)
        w.learned ++= Blocks.all
        w.year = 3
        for(_ <- 0 until 60 * 140) { w.update(1f / 60f); w.effects.clear(); w.sounds.clear(); w.notices.clear() }
        val c = w.spawn(UnitKinds.Thistledown, Valley.Bramble, 30f * 32, 26f * 32); c.rallyX = 10000f
        w
      }, -1)); game.step(3); game.play.zoomGoal = 1.5f; game.play.following = false; game.play.centerX = 37f * 32; game.play.centerY = 30f * 32; game.step(60)
    }
    shot("loom") {
      game.load(new PlayScene(() => everything(0), -1)); game.step(3)
      val loom = game.play.world.buildings.collectFirst { case l: LoomBuilding => l }.get
      game.play.world.buildings.filter(b => loom.inReach(b) && !(b eq loom)).take(4).foreach(loom.toggle)
      loom.cards = Vector(Card(Sense.Goods, 2, 0, 200, Doing.Stop, 0, 0), Card(Sense.Season, 0, 2, 4, Doing.Choose, 1, 2), Card(Sense.Foes, 0, 1, 12, Doing.Ring, 0, 0), Card(Sense.Always, 0, 0, 0, Doing.Add, 1, 1), Card(Sense.Drum, 1, 1, 100, Doing.Put, 1, 0), Card(Sense.Health, 1, 0, 50, Doing.Copy, 2, 0))
      game.play.selected = loom; game.step(30); game.play.overlay = Overlay.Loom; game.step(3)
    }
    shot("links") {
      game.load(new PlayScene(() => everything(0), -1)); game.step(3)
      val loom = game.play.world.buildings.collectFirst { case l: LoomBuilding => l }.get
      game.play.world.buildings.filter(b => loom.inReach(b) && !(b eq loom)).take(4).foreach(loom.toggle)
      game.play.selected = loom; game.play.following = false; game.play.centerX = loom.cx; game.play.centerY = loom.cy; game.play.zoomGoal = 1.8f; game.step(60)
    }
    shot("aims") { game.load(new PlayScene(() => MapGen.generate(Valleys.all(1)), 1)); game.step(100) }
    shot("layouts") { game.load(new PlayScene(() => field(0), -1)); game.step(3); game.play.profile.layouts = Vector(Vector(Plan(Blocks.Drill, 0, 0, 0), Plan(Blocks.Belt, 2, 0, 0), Plan(Blocks.Belt, 3, 0, 0), Plan(Blocks.Slinger, 2, 1, 0))); game.play.overlay = Overlay.Layouts; game.step(3) }
    shot("saves") { game.load(new PlayScene(() => MapGen.generate(Valleys.all(1)), 1)); game.step(3); game.play.profile.keepGame(game.play.world, 1); game.play.overlay = Overlay.Saves; game.step(3) }
    for((name, overlay) <- Vector("fr_settings" -> Overlay.Settings, "fr_help" -> Overlay.Help, "fr_menu" -> Overlay.Menu, "fr_codex" -> Overlay.Codex, "fr_waves" -> Overlay.Waves, "fr_ledger" -> Overlay.Ledger, "fr_deeds" -> Overlay.Deeds, "fr_layouts" -> Overlay.Layouts, "fr_saves" -> Overlay.Saves, "fr_learn" -> Overlay.Learn, "fr_end" -> Overlay.End, "fr_keys" -> Overlay.Keys)) shot(name) {
      hearthwheel.core.Lang.language = hearthwheel.core.Lang.Language.French
      game.load(new PlayScene(() => MapGen.generate(Valleys.all(1)), 1)); game.step(3); game.play.codexBlock = Blocks.Loom; game.play.overlay = overlay; game.step(3); game.move(880f, 375f); game.step(2)
    }
    shot("fr_aims") { hearthwheel.core.Lang.language = hearthwheel.core.Lang.Language.French; game.load(new PlayScene(() => MapGen.generate(Valleys.all(0)), 0)); game.step(60); game.play.tool = Tool.Build(Blocks.Drill); game.step(30) }
    shot("fr_valleys") { hearthwheel.core.Lang.language = hearthwheel.core.Lang.Language.French; game.load(ValleyScene); game.step(30) }
    shot("fr_own") { hearthwheel.core.Lang.language = hearthwheel.core.Lang.Language.French; game.load(ValleyScene); game.step(3); game.click(1030f, 40f); game.step(30) }
    shot("fr_loom") {
      hearthwheel.core.Lang.language = hearthwheel.core.Lang.Language.French
      game.load(new PlayScene(() => everything(0), -1)); game.step(3)
      val loom = game.play.world.buildings.collectFirst { case l: LoomBuilding => l }.get
      loom.cards = Vector(Card(Sense.Goods, 2, 0, 200, Doing.Stop, 0, 0), Card(Sense.TurnIn, 0, 2, 4, Doing.Choose, 1, 2), Card(Sense.Foes, 0, 1, 12, Doing.Ring, 0, 0), Card(Sense.Always, 0, 0, 0, Doing.Add, 1, 1))
      game.play.selected = loom; game.step(30); game.play.overlay = Overlay.Loom; game.step(3)
    }
    shot("rules") {
      val m = HandMap.from(MapGen.generate(Valleys.all(3)))
      m.rules = m.rules.copy(name = "Under the hill", aims = Vector(Aim.Deliver(Items.Iron, 200) -> Vector(Stack(Items.Gear, 40)), Aim.Build(Blocks.Boltcaster, 3) -> Vector(Stack(Items.Copper, 200)), Aim.Crew(UnitKinds.Golem, 2) -> Vector.empty, Aim.Reach(2, Season.Winter) -> Vector(Stack(Items.Coal, 100)), Aim.Burn(1) -> Vector.empty))
      game.load(new hearthwheel.core.EditorScene(m, 1)); game.step(4); game.click(98f, 582f); game.step(4)
    }
    shot("help") { game.load(new PlayScene(() => field(0), -1)); game.step(5); game.play.overlay = Overlay.Help; game.step(3) }
    shot("menu") { game.load(new PlayScene(() => field(0), -1)); game.step(5); game.play.overlay = Overlay.Menu; game.step(3) }
    shot("end") { game.load(new PlayScene(() => field(0), -1)); game.step(5); game.play.world.hearth.damage(1e9f); game.step(20) }
    game.stop()
  }
}
