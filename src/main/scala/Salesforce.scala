import scala.annotation.tailrec

object Salesforce extends App {

  case class Component(name: String, parents: Set[Component]) {
    def addParent(parentOpt: Option[Component]) = {
      parentOpt.map(parent => Component(name, (parent :: parents.toList).toSet)).getOrElse(this)
    }
  }

  sealed trait Command

  case object DEPEND extends Command
  case object INSTALL extends Command
  case object REMOVE extends Command
  case object LIST extends Command

  val depends = new scala.collection.mutable.ListBuffer[Component]
  val installed = new scala.collection.mutable.ListBuffer[Component]

  def execute(command: Command, args: String*): Unit = {
    println(s"$command ${args.mkString(" ")}")
    command match {
      case DEPEND =>
        if (args.isEmpty) throw new IllegalArgumentException("DEPEND should have at least one argument")

        add(names = args)
      case INSTALL =>
        if (args.size != 1) throw new IllegalArgumentException("INSTALL should have one and only argument")
        List(install(args(0)))
      case REMOVE =>
        if (args.size != 1) throw new IllegalArgumentException("REMOVE should have one and only argument")
        remove(args(0))
      case LIST => list()
    }
    println()
  }

  private def list(): Unit = {
    installed.foreach(component => println(component.name))
  }

  private def add(names: Seq[String]): List[Component] = {
    def newComponent(name: String, parentOpt: Option[Component]): Component = {
      val component = depends.find(_.name == name).getOrElse(Component(name = name, parents = Set.empty))
      val linkedComponent = component.addParent(parentOpt)
      depends += linkedComponent
      linkedComponent
    }

    @tailrec
    def append(names: List[String], parentOpt: Option[Component], components: List[Component]): List[Component] = {
      names match {
        case Nil => Nil
        case arg :: Nil =>
          newComponent(arg, parentOpt) :: components
        case head :: tail =>
          val component = newComponent(head, parentOpt)
          append(tail, Some(component), component :: components)
      }
    }

    append(names.toList.reverse, None, Nil)
  }

  private def install(name: String): Unit = {
    val component = depends.find(_.name == name).getOrElse(add(List(name)).head)

    if (findInstalledComponent(component.name).isDefined) {
      println(s"${component.name} is already installed")
    } else {
      def append(component: Component): Unit = {
        if (findInstalledComponent(component.name).isEmpty) {
          component.parents.foreach(append)
          println(s"Installing ${component.name}")
          installed += component
        }
      }

      append(component)
    }
  }

  private def remove(name: String): Unit = {
    findInstalledComponent(name) match {
      case None =>
        println(s"$name is not installed")
      case Some(component) =>
        def delete(component: Component): Unit = {
          if (installed.exists(_.parents.exists(_.name == component.name)) && component.name == name)
            println(s"${component.name} is still in use")
          else {
            println(s"Removing ${component.name}")
            installed -= component
            component.parents.foreach(delete)
          }

        }

        delete(component)
    }
  }

  private def findInstalledComponent(name: String): Option[Component] = {
    installed.find(_.name == name)
  }

  execute(DEPEND, "TELNET", "TCPIP", "NETCARD")
  execute(DEPEND, "TCPIP", "NETCARD")
  execute(DEPEND, "DNS", "TCPIP", "NETCARD")
  execute(DEPEND, "BROWSER", "TCPIP", "HTML")

  execute(INSTALL, "NETCARD")
  execute(INSTALL, "TELNET")
  execute(INSTALL, "foo")
  execute(INSTALL, "NETCARD")
  execute(INSTALL, "DNS")
  execute(INSTALL, "BROWSER")

  execute(REMOVE, "TELNET")
  execute(REMOVE, "NETCARD")
  execute(REMOVE, "DNS")
  execute(REMOVE, "NETCARD")
  execute(REMOVE, "TCPIP")
  execute(REMOVE, "BROWSER")
  execute(REMOVE, "TCPIP")

  execute(LIST)

  println("END")
}
