val ProjectName      = "stutter"
val OrganisationName = "splatter"
val ProjectVersion   = "0.1.0"

val ScalaVersion     = "3.10.0"

lazy val root: Project = (project in file("."))
  .settings(
    name         := ProjectName,
    organization := OrganisationName,
    version      := ProjectVersion,
    scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.20.0" % "test",
      "org.scalatest"  %% "scalatest"  % "3.2.20" % "test"
    )
  )

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:strictEquality",
  "-unchecked",
  "-Werror",
  "-deprecation"
)
