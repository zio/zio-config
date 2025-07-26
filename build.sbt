import BuildHelper.*
import sbtcrossproject.CrossProject
import sbtcrossproject.Platform

welcomeMessage

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / excludeLintKeys += ideSkipProject

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.dev/zio-config/")),
    licenses      := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers    := List(
      Developer(
        "afsalthaj",
        "Afsal Thaj",
        "https://medium.com/@afsal.taj06",
        url("https://github.com/afsalthaj")
      ),
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    versionScheme := Some("early-semver")
  )
)

val allPlatforms = Seq(JVMPlatform, JSPlatform, NativePlatform)

addCommandAlias("lint", "; ++2.13; scalafmtSbtCheck; scalafmtCheck; ++3.3; scalafmtCheck")
addCommandAlias("fmt", "; ++2.13; scalafmtSbt; scalafmtAll; ++3.3; scalafmtAll")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")

def addRootPlatformAliases(cmd: String) = {
  val platformAliases = allPlatforms.map { platform =>
    val pName = platform.sbtSuffix
    (
      s"$cmd${platform.sbtSuffix}",
      s"; ++2.12; root2-12$pName/$cmd; ++2.13; root2-13$pName/$cmd; ++3.3; root3$pName/$cmd"
    )
  }.toMap

  val aliases = platformAliases.updated(s"${cmd}All", platformAliases.keys.mkString(";", ";", ";"))
  aliases.toSeq.flatMap { case (alias, cmd) => addCommandAlias(alias, cmd) }
}

addRootPlatformAliases("test")
addRootPlatformAliases("compile")

addCommandAlias(
  "checkMima",
  Seq(
    "zioConfig",
    "zioConfigTypesafe",
    "zioConfigDerivation",
    "zioConfigYaml",
    "zioConfigMagnolia",
    "zioConfigAws",
    "zioConfigZioAws",
    "zioConfigXml"
  ).map(_ + "JVM/mimaReportBinaryIssues").mkString("all ", " ", "")
)

val awsVersion        = "1.12.797"
val zioAwsVersion     = "7.28.29.19"
val zioVersion        = "2.1.24"
val magnoliaVersion   = "0.17.0"
val refinedVersion    = "0.11.3"
val pureconfigVersion = "0.17.8"

lazy val magnoliaDependencies =
  libraryDependencies ++= {
    if (scalaVersion.value == ScalaDotty) Seq.empty // Just to make IntelliJ happy
    else {
      Seq(
        "com.propensive" %% "magnolia"      % magnoliaVersion,
        "org.scala-lang"  % "scala-reflect" % scalaVersion.value
      )
    }
  }

lazy val refinedDependencies =
  libraryDependencies ++= Seq("eu.timepit" %% "refined" % refinedVersion)

lazy val pureconfigDependencies =
  libraryDependencies ++=
    Seq("com.github.pureconfig" %% "pureconfig-core" % pureconfigVersion)

def testSettings = Seq(
  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio-test"     % zioVersion % Test,
    "dev.zio" %%% "zio-test-sbt" % zioVersion % Test
  ),
  testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
)

lazy val allVersionsCrossProjects = Seq(
  zioConfig,
  zioConfigAws,
  zioConfigTypesafe,
  zioConfigDerivation,
  zioConfigYaml,
  zioConfigEnumeratum,
  zioConfigCats,
  zioConfigRefined,
  zioConfigZioAws,
  zioConfigXml,
  zioConfigPureconfig
)

lazy val scala212CrossProjects = allVersionsCrossProjects :+ examples :+ zioConfigTypesafeMagnoliaTests
lazy val scala213CrossProjects = scala212CrossProjects :+ zioConfigScalaz
lazy val scala3CrossProjects   = allVersionsCrossProjects :+ zioConfigScalaz :+ zioConfigMagnolia

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(`root2-13`.componentProjects.flatMap(_.referenced) *)

lazy val `root2-12` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("2-12"))
  .settings(publish / skip := true)
  .aggregate(scala212CrossProjects: _*)
  .configurePlatform(JVMPlatform)(
    _.aggregate(docs, zioConfigMagnoliaJVM)
  )

lazy val `root2-13` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("2-13"))
  .settings(publish / skip := true)
  .aggregate(scala213CrossProjects: _*)
  .configurePlatform(JVMPlatform)(
    _.aggregate(docs, zioConfigMagnoliaJVM)
  )

lazy val root3 = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("3"))
  .settings(publish / skip := true)
  .aggregate(scala3CrossProjects: _*)
  .configurePlatform(JVMPlatform)(
    _.aggregate(docs)
  )

lazy val zioConfig = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(stdSettings("zio-config"))
  .settings(crossProjectSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings("zio.config"))
  .settings(macroDefinitionSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"                %%% "zio"                     % zioVersion,
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.14.0"
    ),
    testSettings
  )
  .nativeSettings(nativeSettings)

lazy val zioConfigWithTests = zioConfig % "compile->compile;test->test"

lazy val zioConfigJVM    = zioConfig.jvm

lazy val zioConfigAws = crossProject(JVMPlatform)
  .in(file("aws"))
  .settings(stdSettings("zio-config-aws"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-ssm" % awsVersion,
      "dev.zio"      %% "zio-streams"      % zioVersion
    ),
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigAwsJVM = zioConfigAws.jvm

lazy val zioConfigZioAws = crossProject(JVMPlatform)
  .in(file("zio-aws"))
  .settings(stdSettings("zio-config-zio-aws"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-aws-ssm" % zioAwsVersion,
      "dev.zio" %% "zio-streams" % zioVersion
    ),
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigZioAwsJVM = zioConfigZioAws.jvm

lazy val zioConfigRefined = crossProject(JVMPlatform)
  .in(file("refined"))
  .settings(stdSettings("zio-config-refined"))
  .settings(crossProjectSettings)
  .settings(
    refinedDependencies,
    testSettings
  )
  .dependsOn(zioConfigMagnolia % "compile->compile;test->test")

lazy val zioConfigRefinedJVM = zioConfigRefined.jvm

lazy val zioConfigPureconfig = crossProject(JVMPlatform)
  .in(file("pureconfig"))
  .settings(stdSettings("zio-config-pureconfig"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    pureconfigDependencies,
    testSettings
  )
  .dependsOn(zioConfigWithTests, zioConfigTypesafe)

lazy val zioConfigPureconfigJVM = zioConfigPureconfig.jvm

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = crossProject(JVMPlatform)
  .in(file("examples"))
  .settings(stdSettings("zio-config-examples"))
  .settings(crossProjectSettings)
  .settings(
    publish / skip := true,
    fork           := true,
    magnoliaDependencies,
    refinedDependencies,
    runAllExamples :=
      Def.taskDyn {
        val classes = (Compile / discoveredMainClasses).value
        val runs    = (Compile / runMain)

        val runTasks = classes.map { cc =>
          Def.task {
            runs.toTask(s" $cc").value
          }
        }

        Def.sequential(runTasks)
      }.value
  )
  .dependsOn(zioConfig, zioConfigMagnolia, zioConfigRefined, zioConfigTypesafe, zioConfigYaml)

lazy val examplesJVM = examples.jvm

lazy val zioConfigDerivation = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("derivation"))
  .settings(stdSettings("zio-config-derivation"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .dependsOn(zioConfig)
  .nativeSettings(nativeSettings)

lazy val zioConfigDerivationJVM = zioConfigDerivation.jvm

// Cross-platform is only for scala 3 where the actual `magnolia` is not used
lazy val zioConfigMagnolia = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("magnolia"))
  .settings(stdSettings("zio-config-magnolia"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    magnoliaDependencies,
    scalacOptions ++= {
      if (scalaVersion.value == ScalaDotty) {
        Seq("-Xmax-inlines", "64")
      } else {
        Seq("-language:experimental.macros")
      }
    },
    testSettings
  )
  .dependsOn(zioConfigWithTests, zioConfigDerivation)
  .nativeSettings(nativeSettings)
  .platformsSettings(JSPlatform, NativePlatform)(
    scalaVersion       := ScalaDotty,
    crossScalaVersions := Seq(ScalaDotty),
    // importing projects triggers suffix conflict because .dependsOn adds both 2.13 and 3.x
    ideSkipProject     := ((Global / scalaVersion).value != ScalaDotty)
  )

lazy val zioConfigMagnoliaJVM = zioConfigMagnolia.jvm

lazy val zioConfigTypesafe = crossProject(JVMPlatform)
  .in(file("typesafe"))
  .settings(stdSettings("zio-config-typesafe"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies += "com.typesafe" % "config" % "1.4.6",
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigTypesafeJVM = zioConfigTypesafe.jvm

lazy val zioConfigYaml = crossProject(JVMPlatform)
  .in(file("yaml"))
  .settings(stdSettings("zio-config-yaml"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "3.0.1",
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigYamlJVM = zioConfigYaml.jvm

lazy val zioConfigXml = crossProject(JVMPlatform)
  .in(file("xml"))
  .settings(stdSettings("zio-config-xml"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies += "dev.zio" %%% "zio-parser" % "0.1.11",
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigXmlJVM = zioConfigXml.jvm

lazy val zioConfigScalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalaz"))
  .settings(stdSettings("zio-config-scalaz"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala212),
    libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.4.0-M15",
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigScalazJVM = zioConfigScalaz.jvm

lazy val zioConfigCats = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("cats"))
  .settings(stdSettings("zio-config-cats"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigEnumeratum = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("enumeratum"))
  .settings(stdSettings("zio-config-enumeratum"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies += "com.beachape" %%% "enumeratum" % "1.9.6",
    testSettings
  )
  .dependsOn(zioConfigWithTests)

lazy val zioConfigTypesafeMagnoliaTests = crossProject(JVMPlatform)
  .in(file("typesafe-magnolia-tests"))
  .settings(stdSettings("zio-config-typesafe-magnolia-tests"))
  .settings(crossProjectSettings)
  .settings(
    publish / skip                       := true,
    libraryDependencies += "com.typesafe" % "config" % "1.4.6",
    testSettings
  )
  .dependsOn(zioConfigWithTests, zioConfigTypesafe, zioConfigMagnolia, zioConfigDerivation)

lazy val docs = project
  .in(file("zio-config-docs"))
  .settings(
    moduleName                                 := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    magnoliaDependencies,
    refinedDependencies,
    crossScalaVersions                         := (zioConfigJVM / crossScalaVersions).value,
    projectName                                := "ZIO Config",
    mainModuleName                             := (zioConfigJVM / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(
        zioConfigJVM,
        zioConfigTypesafeJVM,
        zioConfigDerivationJVM,
        zioConfigYamlJVM,
        zioConfigRefinedJVM,
        zioConfigMagnoliaJVM
      )
  )
  .settings(macroDefinitionSettings)
  .dependsOn(
    zioConfigJVM,
    zioConfigTypesafeJVM,
    zioConfigDerivationJVM,
    zioConfigYamlJVM,
    zioConfigRefinedJVM,
    zioConfigMagnoliaJVM
  )
  .enablePlugins(WebsitePlugin)

lazy val enableMimaSettings =
  Def.settings(
    mimaFailOnProblem      := true,
    mimaPreviousArtifacts  := previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet,
    mimaBinaryIssueFilters := Seq()
  )
