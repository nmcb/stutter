val ProjectName      = "stutter"
val OrganisationName = "splatter"
val ProjectVersion   = "0.1.0"

val ScalaVersion     = "2.12.7"

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
        "com.lihaoyi"    %% "fastparse"  % "2.0.4"
      , "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
      , "org.scalatest"  %% "scalatest"  % "3.0.5"  % "test"
    )
  )
