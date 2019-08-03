val ProjectName      = "stutter"
val OrganisationName = "splatter"
val ProjectVersion   = "0.1.0"

val ScalaVersion     = "2.12.8"

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
        "com.lihaoyi"    %% "fastparse"  % "2.1.2"
      , "org.scalacheck" %% "scalacheck" % "1.14.0" % "test" // pending scala 2.13 release
      , "org.scalatest"  %% "scalatest"  % "3.0.8"  % "test" // pending scala 2.13 release
    )
  )
