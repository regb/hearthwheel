package hearthwheel
package core

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.render2d.Painter2D
import hearthwheel.model._

/** The whole valley in a point per tile, baked and painted again now and then. */
object Minimap {
  def paint(world: Valley, p: Painter2D): Unit = {
    val palette = Palette.all(world.seasonIndex)
    def color(x: Int, y: Int): Int = {
      val b = world.buildingAt(x, y)
      if(b != null) {
        if(b.team == Valley.Bramble) 100
        else if(b.isInstanceOf[HearthBuilding]) 101
        else if(b.isInstanceOf[BeltBuilding]) 102
        else if(b.block.isTurret) 103
        else 104
      } else world.solidAt(x, y) match {
        case Solid.Cliff => 50
        case Solid.Thicket => 51
        case Solid.Open =>
          val ore = world.oreAt(x, y)
          if(world.roots.at(x, y)) 105 else if(world.ventAt(x, y)) 80 else if(ore >= 0) 60 + ore else world.floorAt(x, y).ordinal
      }
    }
    def shown(k: Int): Color = k match {
      case 100 => Tint(0xe0407a)
      case 101 => Tint(0xffd24a)
      case 102 => Tint(0xd8a878)
      case 103 => Tint(0xfff0d2)
      case 105 => Tint(0x7a4462)
      case 104 => Tint(0xf0a860)
      case 50 => Tint.shade(palette.cliff, 0.5f)
      case 51 => Tint.shade(palette.trees(0), 0.6f)
      case 80 => Tint(0xff8a3c)
      case ore if ore >= 60 => Tint(Items.all(ore - 60).color)
      case 0 => palette.deep
      case 1 => palette.shallow
      case f => palette.floors(f)
    }
    // Runs of the same color, one rectangle each.
    var y = 0
    while(y < world.height) {
      var x = 0
      while(x < world.width) {
        val k = color(x, y)
        var n = 1
        while(x + n < world.width && color(x + n, y) == k) n += 1
        p.rect(x.toFloat, y.toFloat, n.toFloat, 1f, shown(k))
        x += n
      }
      y += 1
    }
  }
}
