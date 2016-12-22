val ProjectName      = "stutter"
val OrganisationName = "splatter"
val ProjectVersion  = "0.0.1"

val ScalaVersion     = "2.11.8"

def common: Seq[Setting[_]] = Seq(
  organization := OrganisationName,
  version      := ProjectVersion,
  scalaVersion := ScalaVersion
)

lazy val root = (project in file("."))
  .settings( common: _*)
  .settings(
    name := ProjectName,
    libraryDependencies ++= Seq(
      "com.lihaoyi"       %% "fastparse" % "0.4.1",
      "org.scalatest"     %% "scalatest" % "2.2.6"  % "test",
      "com.lihaoyi"       %  "ammonite"  % "0.8.1"  % "test" cross CrossVersion.full
    )
  )

initialCommands in (Test, console) := """ammonite.Main().run()"""
