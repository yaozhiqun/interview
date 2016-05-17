package airbnb

object Robot extends App {

  sealed trait Direction
  case object N extends Direction
  case object E extends Direction
  case object S extends Direction
  case object W extends Direction

  sealed trait Command {
    def move(from: Position): Position
  }

  case object G extends Command {
    override def move(from: Position): Position = {
      from.direction match {
        case N => from.copy(x = from.x + 1)
        case S => from.copy(x = from.x - 1)
        case E => from.copy(y = from.y + 1)
        case W => from.copy(y = from.y - 1)
      }
    }
  }
  case object L extends Command {
    override def move(from: Position): Position = {
      from.direction match {
        case N => from.copy(direction = W)
        case S => from.copy(direction = E)
        case E => from.copy(direction = N)
        case W => from.copy(direction = S)
      }
    }
  }
  case object R extends Command {
    override def move(from: Position): Position = {
      from.direction match {
        case N => from.copy(direction = E)
        case S => from.copy(direction = W)
        case E => from.copy(direction = S)
        case W => from.copy(direction = N)
      }
    }
  }

  case class Position(x: Int = 0, y: Int = 0, direction: Direction = N)

  def doesCircleExist(commands: String): Boolean = {
    Option(commands) match {
      case Some(cmd) if cmd.length >= 1 && cmd.length <= 2500 =>
      case _ => throw new IllegalArgumentException(s"Illegal commands $commands")
    }
    
    val cmds: Array[Command] = commands.toCharArray.map {
      case 'G' => G
      case 'R' => R
      case 'L' => L
      case x => throw new IllegalArgumentException(s"Unknown command $x")
    }

    val loopCmds = Array.fill(4)(cmds).flatten

    val finalPosition = loopCmds.foldLeft(Position()) { case (current, command) =>
      command.move(current)
    }

    println(s"Final: $finalPosition")

    finalPosition match {
      case (Position(0, 0, _)) => true
      case _ => false
    }
  }

  val commands = "GLGR"
  if (doesCircleExist(commands))
    println("YES")
  else
    println("NO")
}
