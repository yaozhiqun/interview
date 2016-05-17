lazy val root = (project in file(".")).
  settings(
    name := "interview",
    version := "1.0",
    scalaVersion := "2.10.4"
  )

libraryDependencies += "com.twitter" %% "util-core" % "6.19.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test" withSources()
