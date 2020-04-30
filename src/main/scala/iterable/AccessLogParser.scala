package iterable

import scala.io.Source

object AccessLogParser extends App {

  val regex = """^(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2},\d{3}) \[ACCEPTED (DELETE|DEPLOY)\] (.*):(.*)  for client : (_system_|backup-admin) \/ (.*)$""".r

  case class AccessLog(timestamp: String, method: String, repo: String, artifact: String, caller: String, ip: String)

  def parse(filename: String): List[AccessLog] = {
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(filename)).getLines().foldLeft(List[AccessLog]()) {
      case (logs, regex(timestamp, method, repo, artifact, caller, ip)) => AccessLog(timestamp, method, repo, artifact, caller, ip) :: logs
      case (logs, _) => logs
    }
  }

  parse("access.log").foreach(println)

}
