import sbt._
import sbt.Keys._

object CombirianBuild extends Build {

  lazy val combirian = Project(
    id = "combirian",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Combirian",
      organization := "pl.luckboy.combirian",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2"
      // add other settings here
    )
  )
}
