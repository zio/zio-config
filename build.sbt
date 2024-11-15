import Versions.{Scala212, Scala213, Scala3}

enablePlugins(ZioSbtEcosystemPlugin, ZioSbtCiPlugin)

usefulTasksAndSettings ++= BuildHelper.usefulTasksAndSettings.value

Global / onChangedBuildSource := ReloadOnSourceChanges

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    name                      := "zio-config",
    organization              := "dev.zio",
    homepage                  := Some(url("https://zio.dev/zio-config/")),
    licenses                  := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers                := List(
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
    versionScheme             := Some("early-semver"),
    scalaVersion              := Versions.Scala213,
    scala212                  := Versions.Scala212,
    scala213                  := Versions.Scala213,
    scala3                    := Versions.Scala3,
    javaPlatform              := Versions.JdkReleaseVersion,
    ciEnabledBranches         := Seq("master", "series/4.x"),
    ciDefaultJavaDistribution := "temurin",
    ciLintJobs                := ciLintJobs.value.map(job =>
      job.copy(steps =
        job.steps ++ Seq(
          zio.sbt.githubactionsnative.Step.SingleStep(
            name = "Check mima",
            run = Some("sbt checkMima")
          )
        )
      )
    )
  )
)

lazy val zioConfig = projectMatrix
  .in(file("core"))
  .settings(stdSettings(Some("zio-config")))
  .settings(crossProjectSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings("zio.config"))
  .settings(macroDefinitionSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"                %%% "zio"                     % Versions.zio,
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.12.0",
      "dev.zio"                %%% "zio-test"                % Versions.zio % Test,
      "dev.zio"                %%% "zio-test-sbt"            % Versions.zio % Test
    )
  )
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jsSettings)
  .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = nativeSettings)

lazy val zioConfigAws = projectMatrix
  .in(file("aws"))
  .settings(stdSettings(Some("zio-config-aws")))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-ssm" % Versions.aws,
      "dev.zio"      %% "zio-streams"      % Versions.zio,
      "dev.zio"      %% "zio-test"         % Versions.zio % Test,
      "dev.zio"      %% "zio-test-sbt"     % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)

lazy val zioConfigZioAws = projectMatrix
  .in(file("zio-aws"))
  .settings(stdSettings(Some("zio-config-zio-aws")))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-aws-ssm"  % Versions.zioAws,
      "dev.zio" %% "zio-streams"  % Versions.zio,
      "dev.zio" %% "zio-test"     % Versions.zio % Test,
      "dev.zio" %% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)

lazy val zioConfigRefined = projectMatrix
  .in(file("refined"))
  .settings(stdSettings(Some("zio-config-refined")))
  .settings(crossProjectSettings)
  .settings(
    Dependencies.refined,
    libraryDependencies ++=
      Seq(
        "dev.zio" %%% "zio-test"     % Versions.zio % Test,
        "dev.zio" %%% "zio-test-sbt" % Versions.zio % Test
      )
  )
  .dependsOn(zioConfigMagnolia % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jsSettings)
  .nativePlatform(scalaVersions = Seq(Scala3), settings = nativeSettings)

lazy val zioConfigPureconfig = projectMatrix
  .in(file("pureconfig"))
  .settings(stdSettings(Some("zio-config-pureconfig")))
  .settings(crossProjectSettings)
  .settings(
    enableMimaSettings ++ Seq(
      checkMima / skip := true
    )
  )
  .settings(
    Dependencies.pureconfig,
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-test"     % Versions.zio % Test,
        "dev.zio" %% "zio-test-sbt" % Versions.zio % Test
      )
  )
  .dependsOn(zioConfig % "test->test", zioConfigTypesafe)
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = projectMatrix
  .in(file("examples"))
  .settings(stdSettings(Some("zio-config-examples")))
  .settings(crossProjectSettings)
  .settings(
    publish / skip := true,
    fork           := true,
    Dependencies.magnolia,
    Dependencies.refined,
    runAllExamples :=
      Def
        .taskDyn({
          val classes = (Compile / discoveredMainClasses).value
          val runs    = (Compile / runMain)

          val runTasks = classes.map { cc =>
            Def.task {
              runs.toTask(s" $cc").value
            }
          }

          Def.sequential(runTasks)
        })
        .value
  )
  .dependsOn(zioConfig, zioConfigMagnolia, zioConfigRefined, zioConfigTypesafe, zioConfigYaml)
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)

lazy val zioConfigDerivation = projectMatrix
  .in(file("derivation"))
  .settings(stdSettings(Some("zio-config-derivation")))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .dependsOn(zioConfig)
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jsSettings)
  .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = nativeSettings)

lazy val zioConfigMagnolia = projectMatrix
  .in(file("magnolia"))
  .settings(stdSettings(Some("zio-config-magnolia")))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    Dependencies.magnolia,
    scalacOptions ++= {
      if (scalaVersion.value == Scala3)
        Seq.empty
      else
        Seq("-language:experimental.macros")
    },
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-test"     % Versions.zio % Test,
      "dev.zio" %%% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigDerivation)
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jsSettings)
  .nativePlatform(scalaVersions = Seq(Scala3), settings = nativeSettings)

lazy val zioConfigTypesafe = projectMatrix
  .in(file("typesafe"))
  .settings(stdSettings(Some("zio-config-typesafe")))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.3",
      "dev.zio"     %% "zio-test"     % Versions.zio % Test,
      "dev.zio"     %% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)

lazy val zioConfigYaml = projectMatrix
  .in(file("yaml"))
  .settings(stdSettings(Some("zio-config-yaml")))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.snakeyaml" % "snakeyaml-engine" % "2.8",
      "dev.zio"      %% "zio-test"         % Versions.zio % Test,
      "dev.zio"      %% "zio-test-sbt"     % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigXml = projectMatrix
  .in(file("xml"))
  .settings(stdSettings(Some("zio-config-xml")))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-parser"   % "0.1.10",
      "dev.zio" %%% "zio-test"     % Versions.zio % Test,
      "dev.zio" %%% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jsSettings)
// .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = nativeSettings) // Note: zio-parser is not available for Scala Native 0.5

lazy val zioConfigScalaz = projectMatrix
  .in(file("scalaz"))
  .settings(stdSettings(Some("zio-config-scalaz")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core"  % "7.4.0-M15",
      "dev.zio"    %%% "zio-test"     % Versions.zio % Test,
      "dev.zio"    %%% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala213, Scala3), settings = jsSettings)
  .nativePlatform(scalaVersions = Seq(Scala213, Scala3), settings = nativeSettings)

lazy val zioConfigCats = projectMatrix
  .in(file("cats"))
  .settings(stdSettings(Some("zio-config-cats")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"    % "2.12.0",
      "dev.zio"       %%% "zio-test"     % Versions.zio % Test,
      "dev.zio"       %%% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jsSettings)
  .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = nativeSettings)

lazy val zioConfigEnumeratum = projectMatrix
  .in(file("enumeratum"))
  .settings(stdSettings(Some("zio-config-enumeratum")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum"   % "1.7.5",
      "dev.zio"      %%% "zio-test"     % Versions.zio % Test,
      "dev.zio"      %%% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jvmSettings)
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = jsSettings)
  .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = nativeSettings)

lazy val zioConfigTypesafeMagnoliaTests = projectMatrix
  .in(file("typesafe-magnolia-tests"))
  .settings(stdSettings(Some("zio-config-typesafe-magnolia-tests")))
  .settings(crossProjectSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.3",
      "dev.zio"     %% "zio-test"     % Versions.zio % Test,
      "dev.zio"     %% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigTypesafe, zioConfigMagnolia, zioConfigDerivation)
  .jvmPlatform(
    scalaVersions = Seq(Scala212, Scala213 /*, Scala3*/ ),
    settings = jvmSettings
  ) // FIXME: annotations for Scala 3 are not implemented, tests in zioConfigTypesafeMagnoliaTests fail

lazy val docs = projectMatrix
  .in(file("zio-config-docs"))
  .settings(
    moduleName                                 := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    Dependencies.magnolia,
    Dependencies.refined,
    projectName                                := "ZIO Config",
    mainModuleName                             := (zioConfig.jvm(Scala213) / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(
        Seq(zioConfig, zioConfigTypesafe, zioConfigDerivation, zioConfigYaml, zioConfigRefined, zioConfigMagnolia)
          .map(_.jvm(Scala213).project): _*
      )
  )
  .settings(macroDefinitionSettings)
  .jvmPlatform(scalaVersions = Seq(Scala213), settings = jvmSettings)
  .dependsOn(
    zioConfig,
    zioConfigTypesafe,
    zioConfigDerivation,
    zioConfigYaml,
    zioConfigRefined,
    zioConfigMagnolia
  )
  .enablePlugins(WebsitePlugin)

val checkMima = taskKey[Unit]("Check mima")

lazy val enableMimaSettings =
  Def.settings(
    checkMima              := { if (isScalaJVM.value && !(checkMima / skip).value) mimaReportBinaryIssues.value else () },
    mimaFailOnProblem      := true,
    mimaPreviousArtifacts  := previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet,
    mimaBinaryIssueFilters := Seq()
  )
