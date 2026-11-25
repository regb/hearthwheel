package hearthwheel.model

import scala.collection.mutable

/** Buildings joined by pylons. What is made is shared by all that use it. */
final class PowerGraph {
  val members = mutable.ArrayBuffer.empty[Building]
  var produced = 0f
  var needed = 0f
  var stored = 0f
  var capacity = 0f
  /** The part of what is needed that is there, from 0 to 1. */
  var satisfaction = 0f

  def update(dt: Float): Unit = {
    produced = 0f
    needed = 0f
    capacity = 0f
    var i = 0
    while(i < members.size) {
      val b = members(i)
      produced += b.powerOutput
      needed += b.powerDemand
      capacity += b.powerCapacity
      i += 1
    }
    if(stored > capacity) stored = capacity
    if(needed <= 0.0001f) {
      satisfaction = if(produced > 0f || stored > 0f) 1f else 0f
      stored = scala.math.min(capacity, stored + produced * dt)
    } else if(produced >= needed) {
      satisfaction = 1f
      stored = scala.math.min(capacity, stored + (produced - needed) * dt)
    } else {
      val missing = (needed - produced) * dt
      val used = scala.math.min(missing, stored)
      stored -= used
      satisfaction = (produced * dt + used) / (needed * dt)
    }
  }
}

/** Pipes, tanks and culverts that are joined, and what touches them. It holds
  * one fluid at a time: the first to run in, until it has run dry.
  */
final class FluidGraph {
  /** What the fluid runs through. */
  val conduits = mutable.ArrayBuffer.empty[Building]
  /** What gives to it or takes from it. */
  val users = mutable.ArrayBuffer.empty[Building]
  var fluid: Fluid = null
  var supplied = 0f
  var needed = 0f
  var stored = 0f
  var capacity = 0f
  var satisfaction = 0f
  def carries(f: Fluid): Boolean = (fluid eq f) && stored > 0f

  def update(dt: Float): Unit = {
    supplied = 0f
    needed = 0f
    if(fluid != null && stored <= 0.01f) fluid = null
    var i = 0
    while(i < conduits.size) { supply(conduits(i)); i += 1 }
    i = 0
    while(i < users.size) { supply(users(i)); i += 1 }
    if(fluid != null) {
      i = 0
      while(i < users.size) {
        needed += users(i).fluidDemand(fluid)
        i += 1
      }
    }
    stored = scala.math.min(capacity, stored + supplied * dt)
    val want = needed * dt
    if(want <= 0f) satisfaction = if(stored > 0f) 1f else 0f
    else {
      val used = scala.math.min(want, stored)
      stored -= used
      satisfaction = used / want
    }
  }
  private def supply(b: Building): Unit = {
    val made = b.fluidMade
    if(made > 0f) {
      if(fluid == null) fluid = b.fluidKind
      if(fluid eq b.fluidKind) supplied += made
    }
  }
}

/** Keeps the graphs of a world up to date with its buildings. */
final class Networks(world: Valley) {
  val power = mutable.ArrayBuffer.empty[PowerGraph]
  val fluids = mutable.ArrayBuffer.empty[FluidGraph]
  private var powerDirty = true
  private var fluidDirty = true
  private val stack = mutable.ArrayBuffer.empty[Building]

  def changed(b: Building): Unit = {
    if(b.block.usesPower || b.isPowered) powerDirty = true
    if(b.isPlumbed) fluidDirty = true
  }
  def powerChanged(): Unit = powerDirty = true
  def fluidsChanged(): Unit = fluidDirty = true

  def update(dt: Float): Unit = {
    if(powerDirty) rebuildPower()
    if(fluidDirty) rebuildFluids()
    var i = 0
    while(i < power.size) { power(i).update(dt); i += 1 }
    if(dt > 0f) ratchets(dt)
    i = 0
    while(i < fluids.size) { fluids(i).update(dt); i += 1 }
  }

  private def rebuildPower(): Unit = {
    powerDirty = false
    // Stored power follows the flywheels it is in.
    power.foreach { g =>
      val share = if(g.capacity > 0f) g.stored / g.capacity else 0f
      g.members.foreach(b => if(b.powerCapacity > 0f) b.charge = share * b.powerCapacity)
    }
    power.clear()
    world.buildings.foreach(_.power = null)
    world.buildings.foreach { start =>
      if(start.isPowered && start.power == null) {
        val graph = new PowerGraph
        stack.clear()
        start.power = graph
        stack += start
        while(stack.nonEmpty) {
          val b = stack.remove(stack.size - 1)
          graph.members += b
          graph.stored += b.charge
          b.links.foreach { other =>
            if(other.alive && other.power == null) {
              other.power = graph
              stack += other
            }
          }
        }
        power += graph
      }
    }
  }

  // A ratchet evens out what is stored behind it and ahead, one way only.
  private def ratchets(dt: Float): Unit = world.buildings.foreach {
    case r: RatchetBuilding if r.alive =>
      r.flow = 0f
      val from = r.grid(r.back)
      val to = r.grid(r.front)
      if(from != null && to != null && !(from eq to) && from.capacity > 0f && to.capacity > 0f) {
        val a = from.stored / from.capacity
        val b = to.stored / to.capacity
        if(a > b) {
          val moved = scala.math.min((a - b) * scala.math.min(from.capacity, to.capacity) * 0.5f, scala.math.min(30f * dt, from.stored))
          from.stored -= moved
          to.stored = scala.math.min(to.capacity, to.stored + moved)
          r.flow = moved / dt
        }
      }
    case _ => ()
  }

  private def rebuildFluids(): Unit = {
    fluidDirty = false
    // What a pipe held stays in it.
    fluids.foreach { g =>
      val share = if(g.capacity > 0f) g.stored / g.capacity else 0f
      g.conduits.foreach { b => b.charge2 = share * b.fluidCapacity; b.heldFluid = g.fluid }
    }
    fluids.clear()
    world.buildings.foreach { b => b.fluid = null; b.tanks.clear() }
    world.buildings.foreach { start =>
      if(start.alive && start.block.conducts && start.fluid == null) {
        val graph = new FluidGraph
        stack.clear()
        start.fluid = graph
        stack += start
        val amounts = new Array[Float](3)
        while(stack.nonEmpty) {
          val b = stack.remove(stack.size - 1)
          graph.conduits += b
          graph.capacity += b.fluidCapacity
          if(b.heldFluid != null) amounts(b.heldFluid.ordinal) += b.charge2
          def reach(other: Building): Unit = if(other != null && other.alive && other.team == b.team) {
            if(other.block.conducts) {
              if(other.fluid == null) {
                other.fluid = graph
                stack += other
              }
            } else if(other.isPlumbed && !other.tanks.contains(graph)) {
              other.tanks += graph
              graph.users += other
            }
          }
          b.neighbours.foreach(reach)
          b match {
            case c: CulvertBuilding =>
              reach(c.destination)
              c.incoming.foreach(reach)
            case _ => ()
          }
        }
        // Where two fluids meet, the one there is more of stays.
        val most = amounts.indices.maxBy(amounts(_))
        if(amounts(most) > 0f) {
          graph.fluid = Fluid.fromOrdinal(most)
          graph.stored = amounts(most)
        }
        fluids += graph
      }
    }
  }
}
