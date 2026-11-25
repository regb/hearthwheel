package hearthwheel
package core

import hearthwheel.model._

/** Something worth remembering, once it has been done. */
final case class Deed(key: String, label: String, text: String, done: Play => Boolean)

object Deeds {
  private def delivered(p: Play, i: Item, n: Int) = p.world.deliveredCounts(i.id) >= n
  private def has(p: Play, b: Block) = p.world.buildings.exists(x => x.alive && (x.block eq b))
  val all: Vector[Deed] = Vector(
    Deed("first_copper", "First warmth", "Bring copper to the Hearth", delivered(_, Items.Copper, 1)),
    Deed("thousand", "A thousand pounds", "Bring in a thousand copper", delivered(_, Items.Copper, 1000)),
    Deed("bronze", "The bronze age", "Bring in bronze", delivered(_, Items.Bronze, 1)),
    Deed("gear", "Wheels within wheels", "Bring in a gear", delivered(_, Items.Gear, 1)),
    Deed("steel", "Hard feelings", "Bring in steel", delivered(_, Items.Steel, 1)),
    Deed("star", "Fallen star", "Bring in starglass", delivered(_, Items.Starglass, 1)),
    Deed("fed", "Shots fired", "Have ten fed turrets", p => p.world.buildings.count { case t: TurretBuilding => t.role.ammo.nonEmpty && t.hasAmmo; case _ => false } >= 10),
    Deed("slain", "Gardener", "Slay a hundred in one valley", _.world.slain >= 100),
    Deed("slain2", "Head gardener", "Slay a thousand in one valley", _.world.slain >= 1000),
    Deed("ice", "Thin ice", "See the ice take one of them", _.world.iceTook),
    Deed("winter", "Wintered", "Come out of a winter", _.world.year > 1),
    Deed("year3", "Three rings", "Keep a Hearth lit for three years", _.world.year > 3),
    Deed("nest", "Scorched roots", "Burn out a Briar Heart", _.world.nestsBurned > 0),
    Deed("boss", "Old wood", "Fell Old Gnarl", _.world.felled.contains("gnarl")),
    Deed("queen", "Long live", "Bring down the Seed Queen", _.world.felled.contains("queen")),
    Deed("rime", "Spring, at last", "Fell the Rimewood", _.world.felled.contains("rimewood")),
    Deed("wheel", "Set it turning", "Complete the Great Wheel", _.world.longWinter),
    Deed("hearth", "A greater fire", "Grow the Hearth", p => p.world.hearth != null && p.world.hearth.tier > 0),
    Deed("grand", "The greatest fire", "Grow it all the way", p => p.world.hearth != null && p.world.hearth.tier > 1),
    Deed("crew", "Company", "Have twelve automata at once", _.world.crewCount >= 12),
    Deed("colossus", "Colossal", "Make a Colossus or a Skybarge", p => p.world.creatures.exists(c => c.team == Valley.Player && c.kind.tier == 4)),
    Deed("reactor", "Playing with fire", "Run an Ember Reactor", p => p.world.buildings.exists { case r: ReactorBuilding => r.activity > 0.5f; case _ => false }),
    Deed("power", "Bright lights", "Make a hundred power on one grid", _.world.networks.power.exists(_.produced >= 100f)),
    Deed("wagon", "Trade", "Send a wagon out", p => p.world.sentOut.exists(_ > 0) || p.profile.vault.exists(_ > 0)),
    Deed("ride", "In the saddle", "Ride an automaton", _.riding != null),
    Deed("bend", "Bend the year", "Use an Almanac", p => has(p, Blocks.Almanac) && p.world.held > 0f),
    Deed("pod", "Nipped in the bud", "Burn out every pod there is", p => p.world.year > 1 && p.world.rules.wild && p.world.roots.size > 0 && p.world.roots.pods.isEmpty && p.world.slain > 50),
    Deed("boat", "All hands", "Have five boats at once", _.world.creatures.count(c => c.team == Valley.Player && c.kind.floats) >= 5),
    Deed("ice2", "Through the ice", "Sail an icebreaker in winter", p => p.world.frozen && p.world.creatures.exists(c => c.kind.breaksIce && scala.math.abs(c.vx) + scala.math.abs(c.vy) > 5f)),
    Deed("loom", "Weaver", "Have a loom act on a card", _.world.buildings.exists { case l: LoomBuilding => l.acted >= 0; case _ => false }),
    Deed("valley1", "One down", "Win a valley", _.profile.won.nonEmpty),
    Deed("valley5", "Half the region", "Win five valleys", _.profile.won.size >= 5),
    Deed("valleys", "Hearthwheel", "Win every valley", _.profile.won.size >= Valleys.all.size)
  )
  def check(play: Play): Unit = all.foreach { d =>
    if(!play.profile.deeds.contains(d.key) && d.done(play)) {
      play.profile.deeds = play.profile.deeds + d.key
      play.toast(d.label, d.text, "deed")
      play.sound(SoundKind.Deed)
    }
  }
}

/** Small advice, given once, when it is of use. */
object Hints {
  private final case class Hint(key: String, text: Bindings => String, when: Play => Boolean)
  private val all: Vector[Hint] = Vector(
    Hint("turn", b => s"${b.name(Command.Turn)} turns what is in hand. Over a belt with nothing in hand, it turns the belt.", p => p.tool match { case Tool.Build(x) => x.rotates; case _ => false }),
    Hint("shift", _ => "Hold Shift while dragging a belt and it finds its own way.", p => p.world.buildings.count(_.isInstanceOf[BeltBuilding]) > 14),
    Hint("pick", b => s"${b.name(Command.Pick)} picks up whatever the pointer is over.", p => p.world.built > 12),
    Hint("copy", b => s"${b.name(Command.Copy)} copies an area. Layouts can be kept, with ${b.name(Command.Layouts)}.", p => p.world.built > 60),
    Hint("mine", _ => "Click a seam close by to dig it by hand, and a building to hand over what you carry.", p => p.world.time > 25f && p.world.rules.tutorial && p.world.cargoAmount == 0),
    Hint("learn", b => s"There is something new to learn. ${b.name(Command.Learn)} opens the almanac.", p => p.world.time > 60f && Pages.all.exists(p.world.canLearn)),
    Hint("speed", b => s"${b.name(Command.Speed)} makes time go faster, ${b.name(Command.Pause)} stops it. You can plan while it stands still.", p => p.world.time > 150f),
    Hint("summer", _ => "The shallows have dried out. Drills can be put on the seams in the riverbed, until autumn.", p => p.world.dry && p.world.seasonTime > 4f),
    Hint("cold", _ => "It is cold. Machines with frost on them run at half speed: put braziers with coal among them.", p => p.world.frozen && p.world.seasonTime > 4f),
    Hint("flood", _ => "Machines in the riverbed are under water until summer comes again.", p => p.world.buildings.exists(b => b.flooded && b.team == Valley.Player)),
    Hint("fire", _ => "Something burns! Misters put fires out. So do cloudbursts.", _.world.fires.burning.nonEmpty),
    Hint("guardian", _ => "Crowned ones are guardians: slow to fall, and worth felling first.", _.world.creatures.exists(_.guardian)),
    Hint("ride", b => s"${b.name(Command.Ride)} over an automaton lets you ride it. ${b.name(Command.Crew)} commands them.", _.world.crewCount > 0),
    Hint("refit", _ => "Send automata next to a Refit Yard to have them made better.", p => p.world.buildings.exists(_.isInstanceOf[RefitBuilding])),
    Hint("wagon", _ => "Goods in a Wagon Post are yours in every valley after this one.", p => p.world.buildings.exists(_.isInstanceOf[WagonBuilding])),
    Hint("iron", _ => "Iron wants an auger, and an auger wants power.", p => p.world.knows(Blocks.Auger)),
    Hint("reactor", _ => "A reactor without brine gets hot, and then it is gone. And so is whatever stood around it.", p => p.world.buildings.exists(_.isInstanceOf[ReactorBuilding])),
    Hint("roots", _ => "Roots gnaw at what stands in their way. Firepots and other fire clear them, best of all in summer.", p => p.world.year > 1 && p.world.roots.size > 60),
    Hint("pods", _ => "A pod's brood keeps watch around it, and marches with every third wave. Burn pods out early.", _.world.roots.pods.nonEmpty),
    Hint("aims", _ => "Aims on the left are paid for in goods. They are a good order to do things in, not the only one.", p => p.world.aim.nonEmpty && p.world.time > 40f),
    Hint("ice", _ => "Boats are stuck in the ice until spring. Icebreakers are not.", p => p.world.frozen && p.world.creatures.exists(c => c.kind.floats && c.team == Valley.Player)),
    Hint("loom", _ => "Choose the loom, tap what it should act on, then tap the loom again for its cards.", p => p.world.buildings.exists { case l: LoomBuilding => l.cards.isEmpty; case _ => false })
  )
  // About keys, of which fingers have none.
  private val ofKeys = Set("turn", "shift", "pick", "copy", "ride")
  def check(play: Play): Unit = if(play.profile.hints && !play.toasting) {
    all.find(h => !(play.controls.touch && ofKeys(h.key)) && !play.profile.hinted.contains(h.key) && h.when(play)).foreach { h =>
      play.profile.hinted = play.profile.hinted + h.key
      play.toast("", h.text(play.profile.bindings), "learn")
    }
  }
}
