import sbt.Keys._
import coursier._


//moodify your orgnization and version
lazy val commonSettings = Seq(
  scalaVersion in ThisBuild := "2.11.8",
  organization := "me.daoge",
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding",
    "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xfuture",
    "-feature",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused"
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)

lazy val sop = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    //add other settings
    libraryDependencies += "org.typelevel" %% "cats"        % "0.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "0.3",
    libraryDependencies += "org.scalactic" %% "scalactic"   % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest"   % "3.0.1" % "test"
  )