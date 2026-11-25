package hearthwheel.model

/** Something to do on the way to winning a valley. */
enum Aim {
  case Deliver(item: Item, n: Int)
  case Build(block: Block, n: Int)
  case Learn(block: Block)
  case Wave(n: Int)
  case Slay(n: Int)
  case Burn(n: Int)
  case Crew(kind: UnitKind, n: Int)
  case Reach(year: Int, season: Season)
  case Send(n: Int)

  def label: String = this match {
    case Deliver(i, n) => s"Bring $n ${i.label} to the Hearth"
    case Build(b, n) => if(n == 1) s"Build a ${b.label}" else s"Have $n of ${b.label}"
    case Learn(b) => s"Learn the ${b.label}"
    case Wave(n) => s"Hold out until wave $n"
    case Slay(n) => s"Slay $n of the Bramble"
    case Burn(n) => if(n == 1) "Burn out a Briar Heart" else s"Burn out $n Briar Hearts"
    case Crew(k, n) => if(n == 1) s"Have a ${k.label}" else s"Have $n of ${k.label}"
    case Reach(y, s) => s"See the ${s.label.toLowerCase} of year $y"
    case Send(n) => s"Send $n goods out by wagon"
  }
  def goal: Int = this match {
    case Deliver(_, n) => n
    case Build(_, n) => n
    case Learn(_) => 1
    case Wave(n) => n
    case Slay(n) => n
    case Burn(n) => n
    case Crew(_, n) => n
    case Reach(_, _) => 1
    case Send(n) => n
  }
  /** How far along it is, at most `goal`. */
  def done(w: Valley): Int = {
    val v = this match {
      case Deliver(i, _) => w.deliveredCounts(i.id)
      case Build(b, _) => w.buildings.count(x => x.alive && x.team == Valley.Player && (x.block eq b))
      case Learn(b) => if(w.knows(b)) 1 else 0
      case Wave(_) => w.waves.number
      case Slay(_) => w.slain
      case Burn(_) => w.nestsBurned
      case Crew(k, _) => w.creatures.count(c => c.alive && c.team == Valley.Player && (c.kind eq k))
      case Reach(y, s) => if(w.yearsSeen > y || (w.yearsSeen == y && w.seasonsInto >= Aim.into(w.rules, s))) 1 else 0
      case Send(_) => w.wagonsSent
    }
    scala.math.min(goal, v)
  }
}

object Aim {
  /** How many seasons after the first one of a year that one comes. */
  def into(rules: Rules, s: Season): Int = (s.ordinal - rules.startSeason + 4) % 4

  def write(aims: Vector[(Aim, Vector[Stack])], out: Writer): Unit = {
    out.int(aims.size)
    aims.foreach { case (a, reward) =>
      a match {
        case Deliver(i, n) => out.int(0); out.int(i.id); out.int(n)
        case Build(b, n) => out.int(1); out.int(b.id); out.int(n)
        case Learn(b) => out.int(2); out.int(b.id); out.int(0)
        case Wave(n) => out.int(3); out.int(0); out.int(n)
        case Slay(n) => out.int(4); out.int(0); out.int(n)
        case Burn(n) => out.int(5); out.int(0); out.int(n)
        case Crew(k, n) => out.int(6); out.int(k.id); out.int(n)
        case Reach(y, s) => out.int(7); out.int(s.ordinal); out.int(y)
        case Send(n) => out.int(8); out.int(0); out.int(n)
      }
      out.int(reward.size)
      reward.foreach { s => out.int(s.item.id); out.int(s.amount) }
    }
  }
  def read(in: Reader): Vector[(Aim, Vector[Stack])] = Vector.fill(in.int()) {
    val (kind, a, n) = (in.int(), in.int(), in.int())
    val aim = kind match {
      case 0 => Deliver(Items.all(a), n)
      case 1 => Build(Blocks.all(a), n)
      case 2 => Learn(Blocks.all(a))
      case 3 => Wave(n)
      case 4 => Slay(n)
      case 5 => Burn(n)
      case 6 => Crew(UnitKinds.all(a), n)
      case 7 => Reach(n, Season.fromOrdinal(a))
      case _ => Send(n)
    }
    (aim, Vector.fill(in.int())(Stack(Items.all(in.int()), in.int())))
  }
}
