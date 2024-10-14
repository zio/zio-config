import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._
import sbt.Keys._

object Dependencies {
  val magnolia = libraryDependencies ++= {
    if (scalaVersion.value == Versions.Scala3) Seq.empty // Just to make IntelliJ happy
    else {
      Seq(
        "com.propensive" %%% "magnolia"      % Versions.magnolia,
        "org.scala-lang"   % "scala-reflect" % scalaVersion.value
      )
    }
  }

  lazy val refined =
    libraryDependencies ++= Seq("eu.timepit" %%% "refined" % Versions.refined)

  lazy val pureconfig =
    libraryDependencies ++=
      Seq("com.github.pureconfig" %% "pureconfig-core" % Versions.pureconfig)
}
