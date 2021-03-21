import sbt._
import Keys._

import explicitdeps.ExplicitDepsPlugin.autoImport._
import sbtbuildinfo._
import BuildInfoKeys._
import dotty.tools.sbtplugin.DottyPlugin.autoImport._
object BuildHelper {

  val Scala211 = "2.11.12"
  val Scala212 = "2.12.13"
  val Scala213 = "2.13.5"
  val Scala3   = "3.0.0-RC1"

  val SilencerVersion = "1.7.3"

  def testDeps(scalaVersion: String) = 
    if (scalaVersion == Scala211) {
      Seq("org.scalacheck" %% "scalacheck" % "1.15.2" % Test)
    } else {
      Seq("org.scalacheck" %% "scalacheck" % "1.15.3" % Test)
    }

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked"
  )

  private val std2xOptions = Seq(
    "-Xfatal-warnings",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-explaintypes",
    "-Yrangepos",
    "-Xsource:2.13",
    "-Xlint:_,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-deprecation:false"
  )

  val buildInfoSettings = Seq(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, isSnapshot),
    buildInfoPackage := "zio.config",
    buildInfoObject := "BuildInfo"
  )

  private val optimizerOptions = Seq("-opt:l:inline", "-opt-inline-from:zio.internal.**")

  def extraOptions(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) =>
        std2xOptions ++ optimizerOptions
      case Some((2, 12)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:_,imports",
          "-Ywarn-unused:imports",
          "-opt:l:inline",
          "-opt-inline-from:zio.internal.**",
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit"
        ) ++ std2xOptions ++ optimizerOptions
      case Some((2, 11)) =>
        Seq(
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Xexperimental",
          "-Ywarn-unused-import"
        ) ++ std2xOptions
      case _ => Seq.empty
    }

  val silencerOptions =
    Seq(
      scalacOptions ++= {
        if (isDotty.value) {
          Seq.empty
        } else {
          Seq("-P:silencer:lineContentFilters=import VersionSpecificSupport\\._")
        }
      }
    )

  def stdSettings(prjName: String) = Seq(
    name := s"$prjName",
    scalacOptions := stdOptions,
    crossScalaVersions := Seq(Scala213, Scala212, Scala211),
    scalaVersion in ThisBuild := crossScalaVersions.value.head,
    scalacOptions := stdOptions ++ extraOptions(scalaVersion.value),
    libraryDependencies ++= testDeps(scalaVersion.value),
    libraryDependencies ++= {
      if (isDotty.value)
        Seq(
          ("com.github.ghik" % s"silencer-lib_$Scala213" % SilencerVersion % Provided)
            .withDottyCompat(scalaVersion.value)
        )
      else
        Seq(
          compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
          "com.github.ghik" % "silencer-lib" % SilencerVersion % Provided cross CrossVersion.full,
          compilerPlugin("com.github.ghik" % "silencer-plugin" % SilencerVersion cross CrossVersion.full)
        )
    },
    parallelExecution in Test := true,
    incOptions ~= (_.withLogRecompileOnMacro(false)),
    autoAPIMappings := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )

  val scala3Settings = Seq(
    crossScalaVersions += Scala3,
    scalacOptions ++= {
      if (isDotty.value)
        Seq("-noindent")
      else
        Seq()
    },
    sources in (Compile, doc) := {
      val old = (Compile / doc / sources).value
      if (isDotty.value) {
        Nil
      } else {
        old
      }
    },
    parallelExecution in Test := {
      val old = (Test / parallelExecution).value
      if (isDotty.value) {
        false
      } else {
        old
      }
    }
  )
}
