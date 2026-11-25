package hearthwheel

import hearthwheel.model._

/** What it takes to hold a wave: well fed turrets in front of the Hearth, the
  * fewest that get through it. Tells how hard waves are, whoever plays.
  */
object Siege {
  final case class Kit(name: String, blocks: Vector[Block], ammo: Set[Item])
  val kits: Vector[Kit] = Vector(
    Kit("slingers", Vector(Blocks.Slinger), Set(Items.Flint)),
    Kit("slingers+firepots", Vector(Blocks.Slinger, Blocks.Slinger, Blocks.Slinger, Blocks.Emberpot), Set(Items.Flint, Items.Coal)),
    Kit("bolts+firepots+squibs", Vector(Blocks.Boltcaster, Blocks.Emberpot, Blocks.Squib, Blocks.Slinger), Set(Items.Bronze, Items.Coal, Items.Flint)),
    Kit("mortars+bolts+skyracks", Vector(Blocks.MortarRow, Blocks.Boltcaster, Blocks.Skyrack, Blocks.Boltcaster), Set(Items.Powder, Items.Gear))
  )
  def cost(k: Kit, n: Int): Int = (0 until n).map(i => k.blocks(i % k.blocks.size).cost.map(s => s.amount).sum).sum

  /** The share of turrets, by price, that was lost; more than 1 when the Hearth was. */
  def losses(k: Kit, n: Int, wave: Int, season: Season, year: Int, threat: Float, boss: UnitKind = null): Float = {
    val w = new Valley(72, 72, Rules("Siege", "", 72, 5L + wave, Goal.Endless, nests = 1, threat = threat, startSeason = season.ordinal, seasons = Vector(9999f, 9999f, 9999f, 9999f), calm = true, timer = false, wild = false))
    java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
    w.place(Blocks.Hearth, 8, 34, 0, Valley.Player)
    w.place(Blocks.Nest, 60, 34, 0, Valley.Bramble)
    w.learned ++= Blocks.all
    w.year = year
    w.groundField.finish()
    w.climbField.finish()
    // Columns of eight, with room to walk between them, and a lantern to warm them.
    var x = 18
    var y = 28
    var widest = 1
    val turrets = (0 until n).map { i =>
      val b = k.blocks(i % k.blocks.size)
      if(y + b.size > 43) { y = 28; x -= widest + 1; widest = 1 }
      widest = scala.math.max(widest, b.size)
      val made = w.place(b, scala.math.max(12, x), y, 0, Valley.Player)
      y += b.size + (if(i % 3 == 2) 1 else 0)
      made
    }.filter(_.alive).toVector
    w.waves.number = wave - 1
    if(wave > 0) w.waves.launchNow()
    if(boss != null) w.spawn(boss, Valley.Bramble, 56f * 32, 35f * 32)
    var t = 0
    val price = turrets.map(_.block.cost.map(_.amount).sum).sum.toFloat
    while(t < 60 * 200 && w.outcome == Outcome.Playing && (w.waves.foesLeft > 0 || t < 60)) {
      if(t % 20 == 0) turrets.foreach {
        case tu: TurretBuilding if tu.alive => tu.warm = true; tu.role.ammo.foreach { case (item, _) => if(k.ammo(item)) { var m = 0; while(m < 4 && tu.accepts(item, null)) { tu.receive(item, null); m += 1 } } }
        case _ => ()
      }
      w.update(1f / 60f)
      w.effects.clear(); w.sounds.clear(); w.notices.clear()
      t += 1
    }
    if(w.outcome == Outcome.Lost) 2f
    else if(w.outcome == Outcome.Playing && w.waves.foesLeft > 0) 1.5f
    else turrets.filterNot(_.alive).map(_.block.cost.map(_.amount).sum).sum / scala.math.max(1f, price)
  }
  /** The price of the least that holds, or -1. */
  def least(k: Kit, wave: Int, season: Season, year: Int, threat: Float, boss: UnitKind = null): Int = {
    val steps = Vector(2, 4, 6, 8, 12, 16, 22, 30, 40, 56, 72)
    steps.find(n => losses(k, n, wave, season, year, threat, boss) < 0.25f).map(cost(k, _)).getOrElse(-1)
  }
  def main(args: Array[String]): Unit = {
    val threat = args.headOption.map(_.toFloat).getOrElse(0.85f)
    if(args.length > 1) {
      kits.foreach(k => Vector(16, 30, 56, 72).foreach(n => println(s"${args(1)} ${k.name} $n (${cost(k, n)} goods): lost ${losses(k, n, 0, Season.Winter, 2, threat, UnitKinds.byKey(args(1)))}")))
      return
    }
    println(s"Goods spent on turrets to hold a wave, threat $threat")
    println("%-8s %5s  %s".format("season", "wave", kits.map(k => "%23s".format(k.name)).mkString))
    for(season <- Season.values; wave <- Vector(3, 6, 10, 14, 18, 24)) {
      val year = wave / 12 + 1
      println("%-8s %5d  %s".format(season.label, wave, kits.map(k => "%23d".format(least(k, wave, season, year, threat))).mkString))
    }
    for(b <- Vector(UnitKinds.Gnarl, UnitKinds.Queen, UnitKinds.Rimewood)) {
      println("%-14s  %s".format(b.key, kits.map(k => "%23d".format(least(k, 0, if(b eq UnitKinds.Queen) Season.Autumn else Season.Winter, 1, threat, b))).mkString))
    }
  }
}
