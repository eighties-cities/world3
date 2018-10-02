import java.io.PrintWriter

import sbt.IO
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import execnpm.ExecNpmPlugin.autoImport._
import execnpm.NpmDeps._

name := "world3-display"

scalaVersion := "2.12.4"

resolvers += Resolver.bintrayRepo("definitelyscala", "maven")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

lazy val runDisplay = taskKey[Unit]("runDisplay")

lazy val demo = project.in(file(".")) enablePlugins (ExecNpmPlugin) settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2",
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.4",
  libraryDependencies += "fr.iscpif" %%% "scaladget" % "0.9.6-SNAPSHOT",
  libraryDependencies += "com.definitelyscala" %%% "scala-js-plotlyjs" % "1.1.4",
  libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.2",
  npmDeps in Compile += Dep("plotly.js", "1.41.3", List("plotly.min.js")),
  runDisplay := {
    val displayTarget = target.value
    val displayResource = (resourceDirectory in Compile).value
    
    val demoJS = (fastOptJS in Compile).value

    IO.copyFile(demoJS.data, displayTarget / "js/display.js")
    IO.copyFile(dependencyFile.value, displayTarget / "js/deps.js")

    IO.copyFile(displayResource / "display.html", displayTarget / "display.html")
    IO.copyDirectory(displayResource / "js", displayTarget / "js")
  }

)
