package hearthwheel.model

import scala.collection.mutable
import scala.util.Try

/** A map made in the editor. */
final class HandMap(val size: Int) {
  val floors: Array[Byte] = Array.fill(size * size)(Floor.Meadow.ordinal.toByte)
  val ores: Array[Byte] = Array.fill(size * size)(-1)
  val solids = new Array[Byte](size * size)
  var hearthX: Int = size / 2 - 1
  var hearthY: Int = size / 2 - 1
  /** What it is called, what wins it and what is aimed for. */
  var rules: Rules = HandMap.usual
  /** Where Briar Hearts stand, by their top left tile. */
  val nests = mutable.ArrayBuffer.empty[(Int, Int)]

  def save: String = {
    val out = new Writer
    out.int(2); out.int(size); out.int(hearthX); out.int(hearthY)
    out.int(nests.size)
    nests.foreach { case (x, y) => out.int(x); out.int(y) }
    out.bytes(floors); out.bytes(ores); out.bytes(solids)
    RulesCodec.write(rules, out)
    out.result
  }

  /** The valley it stands for. */
  def build(rules: Rules): Valley = {
    val w = new Valley(size, size, rules)
    java.lang.System.arraycopy(floors, 0, w.floors, 0, floors.length)
    java.lang.System.arraycopy(ores, 0, w.ores, 0, ores.length)
    java.lang.System.arraycopy(solids, 0, w.solids, 0, solids.length)
    val rng = new Rng(size * 31L + 7L)
    w.grain.indices.foreach(i => w.grain(i) = (rng.nextInt() & 0x3f).toByte)
    def clear(x: Int, y: Int, n: Int): Unit = for(j <- 0 until n; i <- 0 until n if w.inside(x + i, y + j)) {
      val t = w.tileIndex(x + i, y + j)
      w.solids(t) = 0
      w.ores(t) = -1
      if(Floor.all(w.floors(t)).water) w.floors(t) = Floor.Loam.ordinal.toByte
    }
    clear(hearthX, hearthY, 3)
    w.place(Blocks.Hearth, hearthX, hearthY, 0, Valley.Player)
    rules.start.foreach(s => w.hearth.stock.add(s.item, s.amount))
    if(rules.free) Items.all.foreach(i => w.hearth.stock.add(i, 4000))
    nests.foreach { case (x, y) =>
      clear(x, y, 3)
      w.place(Blocks.Nest, x, y, 0, Valley.Bramble)
    }
    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, w.hearth.cx, w.hearth.cy + 60f)
    if(rules.free) w.learned ++= Blocks.all.filter(_.category.nonEmpty)
    else w.learned ++= Pages.basics
    w.groundField.finish()
    w.climbField.finish()
    w
  }
}
object HandMap {
  val usual: Rules = Rules("", "", 64, 1L, Goal.Cleanse, nests = 1, firstWave = 120f)
  def load(text: String): Try[HandMap] = Try {
    val in = new Reader(text)
    val version = in.int()
    if(version != 1 && version != 2) throw new IllegalArgumentException("A map from another version")
    val m = new HandMap(in.int())
    m.hearthX = in.int()
    m.hearthY = in.int()
    for(_ <- 0 until in.int()) m.nests += ((in.int(), in.int()))
    in.bytes(m.floors); in.bytes(m.ores); in.bytes(m.solids)
    if(version == 2) m.rules = RulesCodec.read(in)
    m
  }
  /** One to start from, out of a seed. */
  def from(w: Valley): HandMap = {
    val m = new HandMap(w.width)
    java.lang.System.arraycopy(w.floors, 0, m.floors, 0, m.floors.length)
    java.lang.System.arraycopy(w.ores, 0, m.ores, 0, m.ores.length)
    java.lang.System.arraycopy(w.solids, 0, m.solids, 0, m.solids.length)
    m.hearthX = w.hearth.x
    m.hearthY = w.hearth.y
    w.nests.foreach(n => m.nests += ((n.x, n.y)))
    m
  }
}
