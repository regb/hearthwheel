package hearthwheel.model

import MathUtil._

/** How automata behave once they are where they were sent. */
enum Stance(val label: String) {
  case Fight extends Stance("Fight")
  case Hold extends Stance("Hold")
  case Patrol extends Stance("Patrol")
}

/** Whatever walks or flies: automata, the lamplighter, and the Bramble. */
final class Creature(val world: Valley, val kind: UnitKind, val team: Int, var x: Float, var y: Float) {
  import Valley.Tile

  var id = 0
  var alive = true
  val maxHp: Float = kind.hp * (if(team != Valley.Bramble) 1f else world.rules.toughness * (if(kind.boss) clamp(world.rules.threat, 0.5f, 1.3f) else 1f))
  var hp: Float = maxHp
  /** Where it was before the last step, to draw what is in between. */
  var beforeX: Float = x
  var beforeY: Float = y
  var vx = 0f
  var vy = 0f
  var rotation = 0f
  var hitAgo = 10f
  /** Distance walked, which legs and wheels follow. */
  var walked = world.rng.float() * 10f
  var reload = 0f
  /** Where it shoots, when it does. */
  var aim = 0f
  var recoil = 0f

  /** Takes hits before health does. */
  var shield: Float = kind.shield
  var maxShield: Float = kind.shield
  /** Led by a guardian's mark: more of a danger, and shown as one. */
  var guardian = false
  /** Steered by the player. */
  var controlled = false
  var driveX = 0f
  var driveY = 0f
  var stance: Stance = Stance.Fight
  /** Keeps to where it was told to be, instead of marching on the Hearth. */
  var guarding = false
  /** Where a patrol began. */
  var patrolX = 0f
  var patrolY = 0f
  /** Further places to go to, one after the other. */
  val route = new scala.collection.mutable.ArrayDeque[(Float, Float)]
  /** What it carries, for those that dig. */
  var cargo: Item = null
  var cargoAmount = 0
  private var work = 0f
  private var digTile = -1

  // Afflictions, in seconds.
  var burn = 0f
  var slow = 0f
  var frozen = 0f
  var chill = 0f
  // Pushes that fade.
  private var px = 0f
  private var py = 0f

  // Orders, for automata.
  var home = -1
  var rallyX = x
  var rallyY = y
  var attackMove = false
  var selected = false
  private var field: FlowField = null
  var fieldTarget = -1

  private var search = world.rng.float() * 0.3f
  private var target: Creature = null
  private var targetBuilding: Building = null
  private var spawnWait = 5f

  def mass: Float = kind.radius * kind.radius / 100f
  def tileX: Int = floor(x / Tile)
  def tileY: Int = floor(y / Tile)
  def tile: Int = tileY * world.width + tileX
  /** Velocity as seen by whoever leads a shot: 0 for x, 1 for y. */
  def vx2(axis: Int): Float = if(frozen > 0f) 0f else if(axis == 0) vx else vy
  def push(dx: Float, dy: Float): Unit = { px += dx; py += dy }

  def hurt(amount: Float, pierceArmor: Boolean = false): Unit = if(alive) {
    var dealt = if(pierceArmor) amount else scala.math.max(amount * 0.25f, amount - kind.armor)
    if(shield > 0f) {
      val taken = scala.math.min(shield, dealt)
      shield -= taken
      dealt -= taken
      if(shield <= 0f) world.fx(FxKind.Ring, x, y, kind.radius, 0x9fe8ff)
    }
    hp -= dealt
    hitAgo = 0f
    if(hp <= 0f) world.killCreature(this)
  }

  private def speedNow: Float = {
    var s = kind.speed * (if(kind.flies && team == Valley.Bramble) world.flyingNow else 1f)
    if(frozen > 0f) return 0f
    if(slow > 0f) s *= 0.55f
    if(kind.floats) {
      if(world.frozen) s *= (if(kind.breaksIce) 0.55f else 0f)
    } else if(!kind.flies) {
      if(world.roots.at(tileX, tileY)) s *= (if(team == Valley.Bramble) 1.2f else 0.75f)
      val f = world.floorAt(tileX, tileY)
      if(f == Floor.Shallow && world.waterOpen) s *= 0.7f
      if(world.season == Season.Winter && team == Valley.Player && !world.isWarm(tileX, tileY)) s *= 0.8f
    }
    s
  }

  def update(dt: Float): Unit = {
    hitAgo += dt
    recoil = scala.math.max(0f, recoil - dt * 5f)
    if(burn > 0f) {
      burn -= dt
      val fierce = if(team == Valley.Bramble && world.season == Season.Summer) 2f else 1f
      hurt((9f + maxHp * 0.006f) * fierce * dt, pierceArmor = true)
      if(!alive) return
      if(world.rng.chance(dt * 8f)) world.fx(FxKind.Flame, x + world.rng.range(-kind.radius, kind.radius) * 0.6f, y + world.rng.range(-kind.radius, kind.radius) * 0.6f, 0f, 0)
    }
    if(slow > 0f) slow -= dt
    if(frozen > 0f) frozen -= dt
    if(chill > 0f) chill = scala.math.max(0f, chill - dt * 0.15f)
    if(team == Valley.Bramble && world.season == Season.Spring && hp < maxHp && burn <= 0f && hitAgo > 2f) hp = scala.math.min(maxHp, hp + maxHp * 0.02f * dt)

    if(kind.shield > 0f && hitAgo > 5f && shield < maxShield) shield = scala.math.min(maxShield, shield + maxShield * 0.05f * dt)
    kind.trade match {
      case Trade.Mends(radius, perSecond) => mend(radius, perSecond, dt)
      case _ => ()
    }
    reload -= dt
    search -= dt
    if(search <= 0f) {
      search = 0.3f
      findTargets()
    }
    if(target != null && !target.alive) target = null
    if(targetBuilding != null && !targetBuilding.alive) targetBuilding = null

    if(frozen > 0f) {
      vx = 0f
      vy = 0f
    }
    else if(UnitKinds.isLantern(kind)) ()
    else if(controlled) drive(driveX, driveY, dt)
    else if(team == Valley.Bramble) think(dt) else serve(dt)

    if(alive) move(dt)
  }

  /** Moves the lamplighter by hand, in a direction of at most 1. */
  def drive(dx: Float, dy: Float, dt: Float): Unit = {
    val k = scala.math.min(1f, dt * 7f)
    val s = if(UnitKinds.isLantern(kind)) kind.speed * 1.5f else speedNow * 1.25f
    vx += (dx * s - vx) * k
    vy += (dy * s - vy) * k
  }

  private def mend(radius: Float, perSecond: Float, dt: Float): Unit = {
    work += dt
    if(work >= 0.5f) {
      work = 0f
      val r = radius * Tile
      world.eachCreature(x, y, r, team) { c =>
        if(!(c eq this) && c.hp < c.maxHp) {
          c.hp = scala.math.min(c.maxHp, c.hp + perSecond * 0.5f)
          world.fx(FxKind.Mend, c.x, c.y, 0.5f, 0)
        }
      }
      world.eachBuilding(x, y, r, team) { b =>
        if(b.hp < b.maxHp && !b.isInstanceOf[SiteBuilding]) {
          b.heal(perSecond * 0.8f)
          world.fx(FxKind.Mend, b.cx, b.cy, b.size * 0.5f, 0)
        }
      }
    }
  }

  // ------------------------------------------------------------- targeting

  def weaponRange: Float = kind.arms match {
    case Arms.Shoots(_, range, _) => range * Tile
    case Arms.Bites(_, _) => kind.radius + 20f
    case Arms.Bursts(_, _) => kind.radius + 12f
    case Arms.Drops(_, _) => 26f
    case Arms.Nothing => 0f
  }

  private def findTargets(): Unit = {
    val range = weaponRange
    kind.arms match {
      case Arms.Nothing =>
        target = null
        targetBuilding = null
      case Arms.Drops(_, _) if team == Valley.Player =>
        target = world.closestCreature(x, y, 9f * Tile, 1 - team, air = false, ground = true)
        targetBuilding = world.closestBuilding(x, y, 9f * Tile, 1 - team)
      case Arms.Drops(_, _) =>
        target = null
        if(targetBuilding == null || !targetBuilding.alive) targetBuilding = world.prey(this, turretsFirst = false)
      case Arms.Shoots(_, _, _) =>
        target = world.closestCreature(x, y, range, 1 - team, air = true, ground = true)
        if(kind.flies && team == Valley.Bramble) {
          if(targetBuilding == null || !targetBuilding.alive) targetBuilding = world.prey(this, turretsFirst = true)
        } else targetBuilding = world.closestBuilding(x, y, range, 1 - team)
      case _ =>
        target = world.closestCreature(x, y, range + 10f, 1 - team, air = false, ground = true)
        targetBuilding = world.closestBuilding(x, y, range + 8f, 1 - team)
    }
  }

  // ------------------------------------------------------------ the Bramble

  private def think(dt: Float): Unit = {
    kind.spawns.foreach { key =>
      spawnWait -= dt
      if(spawnWait <= 0f && hp < maxHp * 0.98f) {
        spawnWait = if(kind.boss) 9f else 7f
        val n = if(kind.boss) 3 else 2
        for(_ <- 0 until n) {
          val c = world.spawn(UnitKinds.byKey(key), team, x + world.rng.range(-10f, 10f), y + world.rng.range(-10f, 10f))
          c.push(world.rng.range(-120f, 120f), world.rng.range(-120f, 120f))
        }
        world.fx(FxKind.Puff, x, y, 0f, 0)
      }
    }
    if(kind eq UnitKinds.Thistledown) {
      if(dist2(x, y, rallyX, rallyY) < sq(20f)) {
        // It has come down. Nothing grows on rock or water.
        if(world.roots.plant(tileX, tileY)) world.notices += Notice("Thistledown has taken root", 0xffb08a)
        world.fx(FxKind.Puff, x, y, 0f, 0)
        alive = false
      } else steer(rallyX, rallyY, speedNow, dt)
    } else if(guarding) guard(dt)
    else if(kind.flies && kind.arms == Arms.Nothing) {
      // Keeps to the others, behind whoever goes first.
      val friend = world.closestWhere(x, y, 14f * Tile, team)(c => !(c eq this) && c.kind.arms != Arms.Nothing)
      if(friend != null) steer(friend.x - cos(friend.rotation) * 30f, friend.y - sin(friend.rotation) * 30f, speedNow, dt)
      else if(world.hearth != null) steer(world.hearth.cx, world.hearth.cy, speedNow * 0.5f, dt)
    } else if(kind.flies) {
      if(targetBuilding != null) {
        val d = dist(x, y, targetBuilding.cx, targetBuilding.cy)
        val stand = kind.arms match {
          case Arms.Drops(_, _) => 4f
          case _ => weaponRange * 0.7f
        }
        if(d > stand) steer(targetBuilding.cx, targetBuilding.cy, speedNow, dt)
        else {
          // Circle around it rather than hang there.
          val a = atan2(y - targetBuilding.cy, x - targetBuilding.cx) + 0.6f
          steer(targetBuilding.cx + cos(a) * stand, targetBuilding.cy + sin(a) * stand, speedNow * 0.6f, dt)
        }
        attackAir()
      } else slowDown(dt)
    } else {
      val blocked = blockingBuilding
      val inTheWay = blocked != null && blocked.team != team
      val nearGoal = targetBuilding != null && targetBuilding.isInstanceOf[HearthBuilding]
      val engaged = kind.arms match {
        case Arms.Shoots(_, _, _) => nearGoal || (inTheWay && targetBuilding != null)
        case _ => false
      }
      if(engaged) slowDown(dt)
      else {
        val f = if(kind.walk == Walk.Climb) world.climbField else world.groundField
        follow(f, speedNow, dt)
      }
      attackGround(if(inTheWay) blocked else null)
    }
  }

  // Goes for whoever comes close, and back.
  private def guard(dt: Float): Unit = {
    val prey = if(target != null) target else world.closestCreature(x, y, 8f * Tile, 1 - team, air = !kind.arms.isInstanceOf[Arms.Bites], ground = true)
    val far = dist2(x, y, rallyX, rallyY) > sq(9f * Tile)
    if(prey != null && !far) {
      if(dist2(x, y, prey.x, prey.y) > sq(weaponRange * 0.7f)) steer(prey.x, prey.y, speedNow, dt) else slowDown(dt)
      if(kind.flies) attackAir() else attackGround(null)
    } else if(dist2(x, y, rallyX, rallyY) > sq(30f)) steer(rallyX, rallyY, speedNow * 0.6f, dt)
    else slowDown(dt)
    if(hitAgo < 0.2f && prey == null) guarding = false
  }

  private def attackAir(): Unit = kind.arms match {
    case Arms.Drops(shot, every) =>
      if(reload <= 0f && targetBuilding != null && world.enemyBuildingUnder(x, y, team)) {
        reload = every
        val _ = world.shots.fire(shot, team, x, y, 0f, 0.1f, 0f, air = false, ground = true)
      }
    case Arms.Shoots(_, _, _) =>
      val (tx, ty, ok) =
        if(target != null) (target.x, target.y, true)
        else if(targetBuilding != null && dist2(x, y, targetBuilding.cx, targetBuilding.cy) < sq(weaponRange + 20f)) (targetBuilding.cx, targetBuilding.cy, true)
        else (0f, 0f, false)
      if(ok) fireAt(tx, ty)
    case _ => ()
  }

  private def sq(v: Float): Float = v * v

  private def attackGround(blocking: Building): Unit = kind.arms match {
    case Arms.Shoots(_, _, _) =>
      if(blocking != null) fireAt(blocking.cx, blocking.cy)
      else if(target != null) fireAt(target.x, target.y)
      else if(targetBuilding != null) fireAt(targetBuilding.cx, targetBuilding.cy)
    case Arms.Bites(damage, every) =>
      val b = if(blocking != null) blocking else targetBuilding
      if(reload <= 0f) {
        if(target != null && dist2(x, y, target.x, target.y) < sq(kind.radius + target.kind.radius + 10f)) {
          reload = every
          recoil = 1f
          target.hurt(damage)
        } else if(b != null && near(b, kind.radius + 12f)) {
          reload = every
          recoil = 1f
          b.damage(damage * kind.siege)
          world.fx(FxKind.Hit, x + cos(rotation) * kind.radius, y + sin(rotation) * kind.radius, rotation, 0xc9a06a)
          world.sound(SoundKind.Chomp, x, y)
        }
      }
    case Arms.Bursts(damage, radius) =>
      val hitBuilding = (blocking != null && near(blocking, kind.radius + 6f)) || (targetBuilding != null && near(targetBuilding, kind.radius + 6f))
      val hitUnit = target != null && dist2(x, y, target.x, target.y) < sq(kind.radius + target.kind.radius + 6f)
      if(hitBuilding || hitUnit) {
        world.explode(x, y, radius, damage, team, 0f, 0f, air = false, ground = true, 0xa6d86a)
        world.killCreature(this)
      }
    case Arms.Drops(_, _) | Arms.Nothing => ()
  }

  private def near(b: Building, reach: Float): Boolean = {
    val h = b.size * Tile / 2f
    val dx = scala.math.max(0f, scala.math.abs(x - b.cx) - h)
    val dy = scala.math.max(0f, scala.math.abs(y - b.cy) - h)
    dx * dx + dy * dy <= reach * reach
  }

  def fireAt(tx: Float, ty: Float): Unit = kind.arms match {
    case Arms.Shoots(shot, range, every) =>
      val wanted = atan2(ty - y, tx - x)
      aim = wanted
      if(reload <= 0f && frozen <= 0f) {
        reload = every * (if(slow > 0f) 1.3f else 1f)
        recoil = 1f
        val mx = x + cos(wanted) * kind.radius
        val my = y + sin(wanted) * kind.radius
        val angle = wanted + world.rng.range(-0.05f, 0.05f)
        val f = world.shots.fire(shot, team, mx, my, angle, if(shot.lob) dist(mx, my, tx, ty) else 0f, range * Tile * (if(shot.homing > 0f) 1.5f else 1.15f), air = !shot.lob, ground = true)
        f.prey = target
        world.fx(FxKind.Muzzle, mx, my, angle, shot.color)
        world.sound(if(team == Valley.Bramble) SoundKind.Sting else SoundKind.Spark, x, y)
      }
    case _ => ()
  }

  // --------------------------------------------------------------- automata

  private def serve(dt: Float): Unit = kind.trade match {
    case Trade.Digs(perItem) => dig(perItem, dt)
    case Trade.Builds if rallied && world.builder.active != null && world.lantern != null =>
      // Keeps close to the work, and shoots what comes there.
      val site = world.builder.active
      steer(site.cx + cos(id.toFloat) * 50f, site.cy + sin(id.toFloat) * 50f, speedNow, dt)
      if(target != null) fireAt(target.x, target.y)
    case _ => soldier(dt)
  }
  /** Whether it has got where it was sent, or was never sent anywhere. */
  private def rallied: Boolean = stance != Stance.Hold

  private def soldier(dt: Float): Unit = {
    val chases = stance != Stance.Hold && (attackMove || stance == Stance.Patrol || dist2(x, y, rallyX, rallyY) < sq(5f * Tile))
    val goal: (Float, Float) =
      if(target != null) (target.x, target.y)
      else if(targetBuilding != null) (targetBuilding.cx, targetBuilding.cy)
      else null
    if(goal != null) kind.arms match {
      case Arms.Drops(shot, every) =>
        if(reload <= 0f && dist2(x, y, goal._1, goal._2) < sq(30f)) {
          reload = every
          val _ = world.shots.fire(shot, team, x, y, 0f, 0.1f, 0f, air = false, ground = true)
        }
      case _ => fireAt(goal._1, goal._2)
    }
    val bombs = kind.arms.isInstanceOf[Arms.Drops] && goal != null && chases
    val d2 = dist2(x, y, rallyX, rallyY)
    val arrived = d2 < sq(kind.radius + 14f)
    if(arrived && route.nonEmpty) {
      val next = route.removeHead()
      rallyX = next._1
      rallyY = next._2
    } else if(arrived && stance == Stance.Patrol) {
      val (bx, by) = (patrolX, patrolY)
      patrolX = rallyX
      patrolY = rallyY
      rallyX = bx
      rallyY = by
    }
    val hold = goal != null && chases && dist2(x, y, goal._1, goal._2) < sq(weaponRange * 0.85f)
    if(bombs) steer(goal._1, goal._2, speedNow, dt)
    else if(arrived || hold) slowDown(dt)
    else if(kind.flies) steer(rallyX, rallyY, speedNow, dt)
    else if(d2 < sq(1.5f * Tile)) steer(rallyX, rallyY, speedNow, dt)
    else {
      val wanted = world.tileIndex(floor(rallyX / Tile), floor(rallyY / Tile))
      if(field == null || fieldTarget != wanted) {
        fieldTarget = wanted
        field = world.fieldTo(wanted, kind.walk)
      }
      follow(field, speedNow, dt)
    }
  }

  // Back and forth between a seam and the Hearth.
  private def dig(perItem: Float, dt: Float): Unit = {
    val h = world.hearth
    if(h == null) { slowDown(dt); return }
    if(cargoAmount >= 12 || (cargoAmount > 0 && digTile < 0)) {
      if(dist2(x, y, h.cx, h.cy) < sq(70f)) {
        h.stock.add(cargo, cargoAmount)
        world.delivered(cargo, cargoAmount)
        world.line(FxKind.Leaf, x, y, h.cx, h.cy, 0f, cargo.color)
        cargoAmount = 0
      } else steer(h.cx, h.cy, speedNow, dt)
    } else {
      if(digTile < 0 || world.oreTile(digTile) < 0 || world.buildingAt(digTile % world.width, digTile / world.width) != null) {
        // Whatever the Hearth has the least of, of what is soft and close.
        work += dt
        if(work > 0.5f) { work = 0f; digTile = world.oreToDig(this) }
      }
      if(digTile < 0) steer(rallyX, rallyY, speedNow, dt)
      else {
        val ox = (digTile % world.width + 0.5f) * Tile
        val oy = (digTile / world.width + 0.5f) * Tile
        val item = Items.all(world.oreTile(digTile))
        if(cargoAmount > 0 && !(cargo eq item)) { cargoAmount = 0 }
        if(dist2(x, y, ox, oy) < sq(34f)) {
          slowDown(dt)
          aim = atan2(oy - y, ox - x)
          rotation = turnTo(rotation, aim, dt * 5f)
          work += dt
          if(work >= perItem * item.hardness) {
            work = 0f
            cargo = item
            cargoAmount += 1
            world.gathered(item, 1)
            world.fx(FxKind.Hit, ox, oy, aim + Pi, item.color)
          }
        } else steer(ox, oy, speedNow, dt)
      }
    }
  }
  def digging: Boolean = digTile >= 0 && kind.trade.isInstanceOf[Trade.Digs] && cargoAmount < 12

  // ----------------------------------------------------------------- moving

  private def steer(tx: Float, ty: Float, speed: Float, dt: Float): Unit = {
    val d = dist(x, y, tx, ty)
    if(d > 0.5f) {
      val want = scala.math.min(speed, d * 4f)
      val wx = (tx - x) / d * want
      val wy = (ty - y) / d * want
      val k = scala.math.min(1f, dt * (if(kind.flies) 4f else 8f))
      vx += (wx - vx) * k
      vy += (wy - vy) * k
    }
  }
  private def slowDown(dt: Float): Unit = {
    val k = scala.math.min(1f, dt * 6f)
    vx -= vx * k
    vy -= vy * k
  }

  private def follow(f: FlowField, speed: Float, dt: Float): Unit = {
    val tx = clamp(tileX, 0, world.width - 1)
    val ty = clamp(tileY, 0, world.height - 1)
    val next = f.next(tx, ty)
    if(next < 0) slowDown(dt)
    else {
      val nx = (next % world.width + 0.5f) * Tile
      val ny = (next / world.width + 0.5f) * Tile
      val d = scala.math.max(1f, dist(x, y, nx, ny))
      val k = scala.math.min(1f, dt * 7f)
      vx += ((nx - x) / d * speed - vx) * k
      vy += ((ny - y) / d * speed - vy) * k
    }
  }

  /** The building it would bump into on its way. */
  private def blockingBuilding: Building = {
    val f = if(kind.walk == Walk.Climb) world.climbField else world.groundField
    val next = f.next(clamp(tileX, 0, world.width - 1), clamp(tileY, 0, world.height - 1))
    if(next < 0) null
    else {
      val b = world.buildingAt(next % world.width, next / world.width)
      if(b != null && near(b, kind.radius + 14f)) b else null
    }
  }

  private def move(dt: Float): Unit = {
    val mx = (vx + px) * dt
    val my = (vy + py) * dt
    val fade = scala.math.max(0f, 1f - dt * 8f)
    px *= fade
    py *= fade
    if(kind.flies) {
      x = clamp(x + mx, 0f, world.width * Tile - 1f)
      y = clamp(y + my, 0f, world.height * Tile - 1f)
    } else {
      // Each axis by itself, so that it slides along what is in the way. The
      // body of anything big is kept small here, or it could not take a corner.
      val r = scala.math.min(kind.radius, 11f)
      if(mx != 0f) {
        val edge = x + mx + (if(mx > 0f) r else -r)
        if(!world.blocks(this, floor(edge / Tile), floor(y / Tile))) x += mx else vx *= 0.5f
      }
      if(my != 0f) {
        val edge = y + my + (if(my > 0f) r else -r)
        if(!world.blocks(this, floor(x / Tile), floor(edge / Tile))) y += my else vy *= 0.5f
      }
      x = clamp(x, 1f, world.width * Tile - 1f)
      y = clamp(y, 1f, world.height * Tile - 1f)
      // A boat on dry land waits for the water.
      if(!kind.floats && world.blocks(this, tileX, tileY)) escape()
    }
    val moved = dist(0f, 0f, mx, my)
    walked += moved
    if(moved > 0.05f * dt * 60f * 0.1f) {
      val wanted = atan2(vy, vx)
      if(scala.math.abs(vx) + scala.math.abs(vy) > 4f) rotation = turnTo(rotation, wanted, dt * 6f)
    } else if(reload > 0f && !UnitKinds.isLantern(kind)) rotation = turnTo(rotation, aim, dt * 5f)
  }

  // Something was built, or thawed, where it stands.
  private def escape(): Unit = {
    var r = 1
    while(r < 6) {
      var dy = -r
      while(dy <= r) {
        var dx = -r
        while(dx <= r) {
          if(scala.math.max(scala.math.abs(dx), scala.math.abs(dy)) == r && !world.blocks(this, tileX + dx, tileY + dy) && world.inside(tileX + dx, tileY + dy)) {
            val nx = (tileX + dx + 0.5f) * Tile
            val ny = (tileY + dy + 0.5f) * Tile
            x += (nx - x) * 0.2f
            y += (ny - y) * 0.2f
            return
          }
          dx += 1
        }
        dy += 1
      }
      r += 1
    }
  }

  def write(out: Writer): Unit = {
    out.int(kind.id); out.int(team); out.float(x); out.float(y); out.float(hp); out.float(rotation)
    out.float(burn); out.float(slow); out.int(home); out.float(rallyX); out.float(rallyY)
    out.int(if(attackMove) 1 else 0)
    out.float(shield); out.float(maxShield); out.int(if(guardian) 1 else 0); out.int(stance.ordinal)
    out.int(if(guarding) 1 else 0)
    out.float(patrolX); out.float(patrolY); out.int(if(cargo == null) -1 else cargo.id); out.int(cargoAmount)
  }
}
object Creature {
  def read(world: Valley, in: Reader): Creature = {
    val kind = UnitKinds.all(in.int())
    val team = in.int()
    val c = new Creature(world, kind, team, in.float(), in.float())
    c.hp = in.float()
    c.rotation = in.float()
    c.burn = in.float()
    c.slow = in.float()
    c.home = in.int()
    c.rallyX = in.float()
    c.rallyY = in.float()
    c.attackMove = in.int() == 1
    c.shield = in.float(); c.maxShield = in.float(); c.guardian = in.int() == 1; c.stance = Stance.fromOrdinal(in.int())
    c.guarding = in.int() == 1
    c.patrolX = in.float(); c.patrolY = in.float()
    val cargo = in.int()
    c.cargo = if(cargo < 0) null else Items.all(cargo)
    c.cargoAmount = in.int()
    c
  }
}

/** A projectile in flight. Reused once spent. */
final class Flying {
  var shot: Shot = null
  var team = 0
  var x = 0f
  var y = 0f
  var vx = 0f
  var vy = 0f
  var angle = 0f
  var age = 0f
  var life = 0f
  var pierce = 0
  var air = true
  var ground = true
  /** Total flight time of a lobbed shot, 0 for a straight one. */
  var lobbed = 0f
  /** Swatted: gone before it gets anywhere. */
  var spent = false
  /** Whom it follows, if it can. */
  var prey: Creature = null
  // Those it went through, so as not to hit them again.
  val struck = new Array[Int](8)
  var struckCount = 0
}

/** Every projectile of the world. */
final class Shots(world: Valley) {
  import Valley.Tile
  private val pool = new scala.collection.mutable.ArrayBuffer[Flying]
  val flying = new scala.collection.mutable.ArrayBuffer[Flying]

  /** @param lobDistance for a lobbed shot, how far it lands */
  def fire(shot: Shot, team: Int, x: Float, y: Float, angle: Float, lobDistance: Float, range: Float, air: Boolean, ground: Boolean): Flying = {
    val f = if(pool.nonEmpty) pool.remove(pool.size - 1) else new Flying
    f.shot = shot
    f.team = team
    f.x = x
    f.y = y
    f.angle = angle
    f.vx = cos(angle) * shot.speed
    f.vy = sin(angle) * shot.speed
    f.age = 0f
    f.pierce = shot.pierce
    f.air = air
    f.ground = ground
    f.struckCount = 0
    f.spent = false
    f.prey = null
    if(shot.lob) {
      f.lobbed = scala.math.max(0.25f, lobDistance / scala.math.max(1f, shot.speed))
      f.life = f.lobbed
    } else if(shot.speed <= 0f) {
      f.lobbed = shot.life
      f.life = shot.life
    } else {
      f.lobbed = 0f
      f.life = if(range > 0f) range / shot.speed else shot.life
    }
    flying += f
    f
  }

  def clear(): Unit = { pool ++= flying; flying.clear() }

  def update(dt: Float): Unit = {
    var i = 0
    while(i < flying.size) {
      val f = flying(i)
      if(step(f, dt)) i += 1
      else {
        val last = flying.remove(flying.size - 1)
        if(!(last eq f)) flying(i) = last
        pool += f
      }
    }
  }

  /** The closest shot of a team. */
  def closest(x: Float, y: Float, r: Float, team: Int): Flying = {
    var best: Flying = null
    var bestDistance = r * r
    var i = 0
    while(i < flying.size) {
      val f = flying(i)
      if(f.team == team && !f.spent && f.shot.speed > 0f) {
        val d = dist2(x, y, f.x, f.y)
        if(d < bestDistance) { best = f; bestDistance = d }
      }
      i += 1
    }
    best
  }

  // Whether it flies on.
  private def step(f: Flying, dt: Float): Boolean = {
    f.age += dt
    if(f.spent) return false
    if(f.shot.homing > 0f) {
      if(f.prey == null || !f.prey.alive) f.prey = world.closestCreature(f.x, f.y, 160f, 1 - f.team, f.air, f.ground)
      if(f.prey != null) {
        f.angle = turnTo(f.angle, atan2(f.prey.y - f.y, f.prey.x - f.x), f.shot.homing * dt)
        f.vx = cos(f.angle) * f.shot.speed
        f.vy = sin(f.angle) * f.shot.speed
      }
    }
    if(world.shielded(f)) return false
    f.x += f.vx * dt
    f.y += f.vy * dt
    if(f.lobbed > 0f) {
      if(f.age >= f.life) { land(f); false } else true
    } else if(f.age >= f.life) {
      if(f.shot.splash > 0f) land(f)
      false
    } else if(f.x < 0f || f.y < 0f || f.x >= world.width * Tile || f.y >= world.height * Tile) false
    else {
      val c = world.hitCreature(f)
      if(c != null) {
        hit(f, c)
        if(f.shot.splash > 0f) { land(f); false }
        else {
          f.pierce -= 1
          if(f.struckCount < f.struck.length) { f.struck(f.struckCount) = c.id; f.struckCount += 1 }
          f.pierce >= 0
        }
      } else if(f.ground) {
        val b = world.buildingAt(floor(f.x / Tile), floor(f.y / Tile))
        if(b != null && b.team != f.team) {
          if((b.block eq Blocks.StarWall) && f.shot.splash <= 0f && world.rng.chance(0.35f)) {
            // Back where it came from.
            f.team = b.team
            f.angle += Pi + world.rng.range(-0.3f, 0.3f)
            f.vx = cos(f.angle) * f.shot.speed
            f.vy = sin(f.angle) * f.shot.speed
            f.age = 0f
            f.x += f.vx * dt * 2f
            f.y += f.vy * dt * 2f
            world.fx(FxKind.Hit, f.x, f.y, f.angle, 0xc9a8ff)
            return true
          }
          if(f.shot.splash > 0f) land(f)
          else {
            b.damage(f.shot.damage)
            if(f.shot.burn > 0f) world.ignite(b, 0.1f)
            world.fx(FxKind.Hit, f.x, f.y, f.angle + Pi, f.shot.color)
          }
          false
        } else true
      } else true
    }
  }

  private def hit(f: Flying, c: Creature): Unit = {
    c.hurt(f.shot.damage)
    if(f.shot.burn > 0f) c.burn = scala.math.max(c.burn, f.shot.burn)
    if(f.shot.slow > 0f) c.slow = scala.math.max(c.slow, f.shot.slow)
    if(f.shot.chill > 0f) c.slow = scala.math.max(c.slow, f.shot.chill)
    if(f.shot.knock > 0f) c.push(cos(f.angle) * f.shot.knock / c.mass, sin(f.angle) * f.shot.knock / c.mass)
    world.fx(FxKind.Hit, f.x, f.y, f.angle + Pi, f.shot.color)
  }

  private def land(f: Flying): Unit =
    world.explode(f.x, f.y, f.shot.splash, f.shot.splashDamage, f.team, f.shot.burn, f.shot.slow, f.air, f.ground, f.shot.color)
}
