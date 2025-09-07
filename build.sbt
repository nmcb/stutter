val ProjectName      = "stutter"
val OrganisationName = "splatter"
val ProjectVersion   = "0.1.0"

val ScalaVersion     = "3.7.2"

def common: Seq[Setting[_]] = Seq(
    organization := OrganisationName
  , version      := ProjectVersion
  , scalaVersion := ScalaVersion
)

lazy val root: Project = (project in file("."))
  .settings( common: _* )
  .settings(
    name := ProjectName,
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.19.0" % "test",
      "org.scalatest"  %% "scalatest"  % "3.2.19" % "test"
    )
  )
