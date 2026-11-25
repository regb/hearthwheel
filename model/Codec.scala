package hearthwheel.model

import scala.util.Try

/** Layouts of buildings, as text to keep. */
object LayoutCodec {
  def save(plans: Vector[Plan]): String = {
    // By name, so that they outlive changes to what can be built.
    plans.map(p => Vector(p.block.key, p.x, p.y, p.rotation, p.config, p.note).mkString(" ")).mkString("|")
  }
  def load(text: String): Option[Vector[Plan]] = Try {
    text.split('|').toVector.map { t =>
      val f = t.split(" ", 6)
      val block = Blocks.byKey(f(0))
      if(block.category.isEmpty) throw new IllegalArgumentException("Not to be built")
      Plan(block, f(1).toInt, f(2).toInt, f(3).toInt & 3, f(4).toInt, note = if(f.length > 5) f(5) else "")
    }
  }.toOption.filter(_.nonEmpty).filter(_.size <= 4000)
}

/** Writes a save as numbers separated by spaces. */
final class Writer {
  private val out = new java.lang.StringBuilder
  private def sep(): Unit = if(out.length > 0) { val _ = out.append(' ') }
  def int(v: Int): Unit = { sep(); val _ = out.append(v) }
  /** A text without spaces. */
  def word(v: String): Unit = { sep(); val _ = out.append('=').append(v) }
  def long(v: Long): Unit = { sep(); val _ = out.append(v) }
  /** To a thousandth, which is all that is worth keeping. */
  def float(v: Float): Unit = {
    sep()
    val _ = out.append(scala.math.round(v.toDouble * 1000.0))
  }
  def bytes(values: Array[Byte]): Unit = {
    // Runs of equal values.
    var i = 0
    while(i < values.length) {
      var n = 1
      while(i + n < values.length && values(i + n) == values(i)) n += 1
      int(n)
      int(values(i).toInt)
      i += n
    }
  }
  def stock(s: Stock): Unit = s.amounts.foreach(int)
  def result: String = out.toString
}

final class Reader(text: String) {
  private var at = 0
  def long(): Long = {
    while(at < text.length && text.charAt(at) == ' ') at += 1
    val start = at
    while(at < text.length && text.charAt(at) != ' ') at += 1
    if(start == at) throw new IllegalArgumentException("The save ends too early")
    java.lang.Long.parseLong(text.substring(start, at))
  }
  def int(): Int = long().toInt
  def word(): String = {
    while(at < text.length && text.charAt(at) == ' ') at += 1
    if(at >= text.length || text.charAt(at) != '=') throw new IllegalArgumentException("A word is missing")
    val start = at + 1
    while(at < text.length && text.charAt(at) != ' ') at += 1
    text.substring(start, at)
  }
  def float(): Float = (long() / 1000.0).toFloat
  def bytes(into: Array[Byte]): Unit = {
    var i = 0
    while(i < into.length) {
      val n = int()
      val v = int().toByte
      var k = 0
      while(k < n && i < into.length) { into(i) = v; i += 1; k += 1 }
    }
  }
  def stock(s: Stock): Unit = s.amounts.indices.foreach(i => s.amounts(i) = int())
}

/** Saves and restores a world. */
object Codec {
  private val Version = 3

  def save(w: Valley): String = {
    val out = new Writer
    out.int(Version)
    out.int(w.width)
    out.int(w.height)
    out.bytes(w.floors)
    out.bytes(w.ores)
    out.bytes(w.solids)
    out.bytes(w.grain)
    out.long(w.rng.save)
    out.float(w.time)
    out.int(w.seasonIndex); out.float(w.seasonTime); out.int(w.year); out.float(w.held)
    out.int(if(w.longWinter) 1 else 0)
    w.roots.write(out)
    w.drums.foreach(out.int)
    out.int(w.aimsReached); out.int(w.wagonsSent); out.int(w.nestsBurned)
    out.int(w.built); out.int(w.lost); out.int(w.slain)
    w.gatheredCounts.foreach(out.int)
    w.deliveredCounts.foreach(out.int)
    out.int(w.learned.size)
    w.learned.foreach(b => out.int(b.id))
    w.waves.write(out)
    w.sky.write(out)
    w.sentOut.foreach(out.int)
    out.int(if(w.cargo == null) -1 else w.cargo.id)
    out.int(w.cargoAmount)
    out.int(w.met.size)
    w.met.foreach(k => out.int(k.id))
    out.int(w.fires.burning.size)
    w.fires.burning.foreach { f => out.int(f.x); out.int(f.y) }
    val alive = w.buildings.filter(_.alive).toVector
    out.int(alive.size)
    alive.foreach { b =>
      out.int(b.block.id); out.int(b.x); out.int(b.y); out.int(b.rotation); out.int(b.team)
      b match { case s: SiteBuilding => out.int(s.target.id); case _ => out.int(-1) }
      out.float(b.hp)
      b.write(out)
    }
    // Each link once.
    val index = alive.zipWithIndex.toMap
    val links = for { (b, i) <- alive.zipWithIndex; o <- b.links; j <- index.get(o) if i < j } yield (i, j)
    out.int(links.size)
    links.foreach { case (i, j) => out.int(i); out.int(j) }
    // Crews by the position of their home in the list above.
    out.int(w.creatures.size)
    val homes = alive.zipWithIndex.map { case (b, i) => b.id -> i }.toMap
    w.creatures.foreach { c =>
      out.int(homes.getOrElse(c.home, -1))
      c.write(out)
    }
    out.int(w.ruins.size)
    w.ruins.foreach { r => out.int(r.block.id); out.int(r.x); out.int(r.y); out.int(r.rotation); out.int(r.config); out.word(r.note) }
    out.int(w.builder.plans.size)
    w.builder.plans.foreach { p =>
      out.int(p.block.id); out.int(p.x); out.int(p.y); out.int(p.rotation); out.int(p.config); out.int(if(p.remove) 1 else 0); out.word(p.note)
    }
    out.result
  }

  /** The saved world, unless the save is damaged or from another version. */
  def load(text: String, rules: Rules): Try[Valley] = Try {
    val in = new Reader(text)
    if(in.int() != Version) throw new IllegalArgumentException("A save from another version")
    val w = new Valley(in.int(), in.int(), rules)
    w.loading = true
    in.bytes(w.floors)
    in.bytes(w.ores)
    in.bytes(w.solids)
    in.bytes(w.grain)
    val random = in.long()
    w.time = in.float()
    w.seasonIndex = in.int(); w.seasonTime = in.float(); w.year = in.int(); w.held = in.float()
    w.longWinter = in.int() == 1
    w.roots.read(in)
    w.drums.indices.foreach(i => w.drums(i) = in.int())
    w.aimsReached = in.int(); w.wagonsSent = in.int(); w.nestsBurned = in.int()
    w.built = in.int(); w.lost = in.int(); w.slain = in.int()
    w.gatheredCounts.indices.foreach(i => w.gatheredCounts(i) = in.int())
    w.deliveredCounts.indices.foreach(i => w.deliveredCounts(i) = in.int())
    for(_ <- 0 until in.int()) w.learned += Blocks.all(in.int())
    w.waves.read(in)
    w.sky.read(in)
    w.sentOut.indices.foreach(i => w.sentOut(i) = in.int())
    val cargo = in.int()
    w.cargo = if(cargo < 0) null else Items.all(cargo)
    w.cargoAmount = in.int()
    for(_ <- 0 until in.int()) w.met += UnitKinds.all(in.int())
    for(_ <- 0 until in.int()) w.fires.start(in.int(), in.int())
    val loaded = Vector.fill(in.int()) {
      val block = Blocks.all(in.int())
      val (x, y, rotation, team) = (in.int(), in.int(), in.int(), in.int())
      val target = in.int()
      val hp = in.float()
      val b =
        if(target >= 0) w.make(Blocks.sites(Blocks.all(target).size), x, y, rotation, team, Blocks.all(target))
        else w.make(block, x, y, rotation, team)
      b.read(in)
      w.add(b)
      b.hp = hp
      b
    }
    loaded.foreach(_.onPlaced())
    loaded.foreach { case z: ZiplineBuilding => z.onNeighbours(); case c: CulvertBuilding => c.onNeighbours(); case _ => () }
    for(_ <- 0 until in.int()) w.link(loaded(in.int()), loaded(in.int()))
    for(_ <- 0 until in.int()) {
      val home = in.int()
      val c = Creature.read(w, in)
      c.id = w.freshId()
      if(home >= 0) c.home = loaded(home).id
      w.creatures += c
      if(UnitKinds.isLantern(c.kind)) w.lantern = c
    }
    for(_ <- 0 until in.int()) w.ruins += Ruin(Blocks.all(in.int()), in.int(), in.int(), in.int(), in.int(), in.word())
    for(_ <- 0 until in.int()) w.builder.plans.append(Plan(Blocks.all(in.int()), in.int(), in.int(), in.int(), in.int(), in.int() == 1, in.word()))
    w.loading = false
    // Making things has drawn from it.
    w.rng.restore(random)
    w.groundField.finish()
    w.climbField.finish()
    w
  }
}
