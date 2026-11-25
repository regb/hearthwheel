package hearthwheel
package core

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.render2d.Texture2D
import hearthwheel.model._

/** Every picture, by name and by what it shows. Pictures are 64 pixels to a tile. */
final class Gallery(byName: Map[String, Texture2D]) {
  def apply(name: String): Texture2D = byName.getOrElse(name, throw new NoSuchElementException(s"No picture $name"))
  private def find(name: String): Texture2D = byName.getOrElse(name, null)
  private def series(name: String, n: Int): Array[Texture2D] = Array.tabulate(n)(i => apply(name + i))

  val items: Array[Texture2D] = Items.all.map(i => apply("item_" + i.key)).toArray

  /** What a block stands on, by block. Null for those drawn in their own way. */
  val base: Array[Texture2D] = Blocks.all.map { b =>
    b.role match {
      case _: Role.Turret => apply("base" + b.size)
      case _ => find(b.key)
    }
  }.toArray
  val top: Array[Texture2D] = Blocks.all.map { b =>
    b.key match {
      case "emberpot" => apply("firepot_top")
      case key => find(key + "_top")
    }
  }.toArray
  val hot: Array[Texture2D] = Blocks.all.map(b => find(b.key + "_hot")).toArray

  val belt: Array[Texture2D] = series("belt_s", 8)
  val beltTurn: Array[Texture2D] = series("belt_c", 8)
  val swift: Array[Texture2D] = series("swift_belt_s", 8)
  val swiftTurn: Array[Texture2D] = series("swift_belt_c", 8)
  val covered: Array[Texture2D] = series("covered_belt_s", 8)
  val coveredTurn: Array[Texture2D] = series("covered_belt_c", 8)
  val coveredIn: Texture2D = apply("covered_belt_in")
  val cisternFill: Texture2D = apply("cistern_fill")
  val crop: Texture2D = apply("garden_crop")
  val crate: Texture2D = apply("crate")
  val switchOn: Texture2D = apply("switch_on")
  val switchOff: Texture2D = apply("switch_off")
  val vent: Texture2D = apply("vent")
  val dome: Texture2D = apply("dome")
  val scorch: Array[Texture2D] = series("scorch", 2)
  val splat: Texture2D = apply("splat")
  val cloud: Texture2D = apply("cloud")
  val crown: Texture2D = apply("crown")
  val roots: Array[Texture2D] = series("root", 4)
  val wake: Texture2D = apply("wake")
  val beltIn: Texture2D = apply("belt_in")
  val swiftIn: Texture2D = apply("swift_belt_in")
  val pipeCore: Texture2D = apply("pipe_core")
  val pipeArm: Texture2D = apply("pipe_arm")
  val wheelTop: Array[Texture2D] = series("waterwheel_top", 4)
  val gearBig: Texture2D = apply("gear_big")
  val gearSmall: Texture2D = apply("gear_small")
  val spokes: Array[Texture2D] = series("spoke", 4)
  val hearthRing: Texture2D = apply("hearth_ring")
  val flames: Array[Texture2D] = series("flame", 4)
  val nestHeart: Texture2D = apply("nest_heart")
  val site: Texture2D = apply("site")
  val cracks: Array[Texture2D] = Array(apply("crack1"), apply("crack2"), apply("crack3"))
  val frost: Texture2D = apply("frost")
  val glow: Texture2D = apply("glow")
  val shadow: Texture2D = apply("shadow")
  val white: Texture2D = apply("white")
  val ring: Texture2D = apply("ring")
  val dash: Texture2D = apply("dash")
  val wire: Texture2D = apply("wire")
  val beam: Texture2D = apply("beam")
  val arrow: Texture2D = apply("arrow")
  val corner: Texture2D = apply("corner")
  val cross: Texture2D = apply("cross")
  val boltIcon: Texture2D = apply("bolt_icon")
  val dropIcon: Texture2D = apply("drop_icon")
  val ammoIcon: Texture2D = apply("ammo_icon")

  val floors: Array[Array[Texture2D]] = Array(
    series("f_water", 4), series("f_water", 4), series("f_sand", 4), series("f_gravel", 4), series("f_loam", 4), series("f_meadow", 4)
  )
  val ice: Array[Texture2D] = series("f_ice", 4)
  val bed: Array[Texture2D] = series("f_bed", 4)
  val ripples: Array[Texture2D] = series("ripple", 4)
  val fringeSide: Array[Texture2D] = series("fringe_side", 2)
  val fringeCorner: Array[Texture2D] = series("fringe_corner", 2)
  val shore: Array[Texture2D] = series("shore_side", 2)
  val cliffs: Array[Texture2D] = series("cliff", 4)
  val trees: Array[Texture2D] = series("tree", 3)
  val bare: Array[Texture2D] = series("bare", 3)
  val snowcap: Texture2D = apply("snowcap")
  /** Seams by item, null for goods that are not dug. */
  val ores: Array[Array[Texture2D]] = Items.all.map(i => if(byName.contains(s"ore_${i.key}0")) series("ore_" + i.key, 3) else null).toArray
  val flowers: Array[Texture2D] = series("flower", 4)
  val tufts: Array[Texture2D] = series("tuft", 2)
  val fallen: Array[Texture2D] = series("leaves", 2)
  val drifts: Array[Texture2D] = series("drift", 2)
  val pebbles: Array[Texture2D] = series("pebble", 2)
  val reeds: Array[Texture2D] = series("reed", 2)
  val mushroom: Texture2D = apply("mushroom")

  val puff: Texture2D = apply("puff")
  val spark: Texture2D = apply("spark")
  val chip: Texture2D = apply("chip")
  val leaf: Texture2D = apply("leaf")
  val drop: Texture2D = apply("drop")
  val flake: Texture2D = apply("flake")
  val petal: Texture2D = apply("petal")
  val mote: Texture2D = apply("mote")

  val bodies: Array[Texture2D] = UnitKinds.all.map(k => apply("u_" + k.key)).toArray
  val legBrass: Texture2D = apply("leg_brass")
  val legVine: Texture2D = apply("leg_vine")
  val legWood: Texture2D = apply("leg_wood")
  val legIce: Texture2D = apply("leg_ice")
  val wingPaper: Texture2D = apply("wing_paper")
  val wingGlass: Texture2D = apply("wing_glass")
  val wingLeaf: Texture2D = apply("wing_leaf")
  val wingSail: Texture2D = apply("wing_sail")
  val wingDew: Texture2D = apply("wing_dew")
  private val shotPictures: Map[String, Texture2D] = byName.filter(_._1.startsWith("shot_"))
  private val shotCache = new java.util.IdentityHashMap[Shot, Texture2D]
  def shot(s: Shot): Texture2D = {
    var t = shotCache.get(s)
    if(t == null) {
      t = shotPictures(s.sprite)
      val _ = shotCache.put(s, t)
    }
    t
  }

  val categories: Array[Texture2D] = Array("haul", "gather", "craft", "power", "guard", "season", "crew").map(n => apply("icon_" + n))
  val seasons: Array[Texture2D] = series("season", 4)
  def icon(name: String): Texture2D = apply("icon_" + name)

  /** A single picture of a block, for lists: the base, then what goes over it. */
  def portrait(b: Block): Texture2D = base(b.id)
}

object Tint {
  def apply(rgb: Int, alpha: Float = 1f): Color =
    Color(((rgb >> 16) & 0xff) / 255f, ((rgb >> 8) & 0xff) / 255f, (rgb & 0xff) / 255f, alpha)
  def mix(a: Color, b: Color, t: Float): Color =
    Color(a.red + (b.red - a.red) * t, a.green + (b.green - a.green) * t, a.blue + (b.blue - a.blue) * t, a.alpha + (b.alpha - a.alpha) * t)
  def shade(c: Color, k: Float): Color = Color(c.red * k, c.green * k, c.blue * k, c.alpha)
  private val whites: Array[Color] = Array.tabulate(65)(i => Color(1f, 1f, 1f, i / 64f))
  /** White that is this opaque, out of a few that are kept. */
  def alpha(a: Float): Color = whites(MathUtil.clamp((a * 64f).toInt, 0, 64))

  val Cream: Color = Tint(0xfff0d2)
  val Soft: Color = Tint(0xd8c2a0)
  val Ink: Color = Tint(0x2e2230)
  val Gold: Color = Tint(0xffcf6a)
  val Good: Color = Tint(0x9ce07a)
  val Bad: Color = Tint(0xff7a5c)
  val Water: Color = Tint(0x7cc8f0)
}

/** The colors of the land in one season. */
final case class Palette(
    floors: Array[Color],
    deep: Color,
    shallow: Color,
    cliff: Color,
    trees: Array[Color],
    fog: Color
)

object Palette {
  private def floors(sand: Int, gravel: Int, loam: Int, meadow: Int): Array[Color] =
    Array(Color.White, Color.White, Tint(sand), Tint(gravel), Tint(loam), Tint(meadow))

  val all: Array[Palette] = Array(
    Palette(floors(0xe6d29c, 0xb0aba2, 0xb08a60, 0x9bc862), Tint(0x2f7fa8), Tint(0x6cc0cc), Tint(0xb0a6a6), Array(Tint(0x8fc255), Tint(0x7cb04e), Tint(0xf4b4cc)), Tint(0xeaffd8)),
    Palette(floors(0xf6e0a6, 0xc4b59c, 0xc2975f, 0xbfc058), Tint(0x3a94b4), Tint(0xd9bb88), Tint(0xc0ac98), Array(Tint(0x6ba048), Tint(0x5b9040), Tint(0x86ae4c)), Tint(0xfff2c0)),
    Palette(floors(0xd9bf8e, 0xa59a92, 0x9a6f4e, 0xbd9a50), Tint(0x2f6a8c), Tint(0x66a0aa), Tint(0xa8988f), Array(Tint(0xe0873a), Tint(0xcf5a34), Tint(0xecbb4a)), Tint(0xffd8a0)),
    Palette(floors(0xe8e8ee, 0xc6cdd8, 0xd8dfe8, 0xecf3f9), Tint(0x96c4de), Tint(0xc4e4f0), Tint(0xa6aebf), Array(Color.White, Color.White, Color.White), Tint(0xd8ecff))
  )
}
