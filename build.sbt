import Versions.{Scala212, Scala213, Scala3}
import BuildHelper.jvmSettings

enablePlugins(ZioSbtCiPlugin, ZioSbtEcosystemPlugin)

name := "zio-config"

onLoadMessage := onLoadMessage.value + BuildHelper.welcomeMessage

inThisBuild(
  List(
    organization := "dev.zio",
    name         := "zio-config",
    homepage     := Some(url("https://zio.dev/zio-config/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
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
    scalafixDependencies ++= List(
      "com.github.liancheng" %% "organize-imports" % "0.6.0"
    ),
    scalacOptions ++= Seq(s"-release:${Versions.JdkReleaseVersion}") ++ {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Seq(
            "-Xmax-inlines:64"
          )
        case _            => Seq.empty
      }
    },
    scala3       := Versions.Scala3,
    scala212     := Versions.Scala212,
    scala213     := Versions.Scala213
  )
)

addCommandAlias("fmt", "; scalafmtSbt; scalafmt; test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("compileAll", "; +compile;")
addCommandAlias("testAll", "; +test;")
addCommandAlias(
  "testJS",
  ";zioConfigJS/test"
)
addCommandAlias(
  "testJVM212",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;examplesJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test;zioConfigPureconfigJVM/test"
)
addCommandAlias(
  "testJVM213",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigRefinedJVM/test;zioConfigMagnoliaJVM/test;examplesJVM/test;zioConfigTypesafeMagnoliaTestsJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test;zioConfigPureconfigJVM/test"
)
addCommandAlias(
  "testJVM3x",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigMagnoliaJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test;zioConfigPureconfigJVM/test"
)
addCommandAlias(
  "testJVM",
  ";testJVM212;testJVM213;testJVM3x;"
)

val awsVersion        = "1.12.721"
val zioAwsVersion     = "5.19.33.2"
val zioVersion        = "2.0.13"
val magnoliaVersion   = "0.17.0"
val refinedVersion    = "0.11.1"
val pureconfigVersion = "0.16.0"
val shapelessVersion  = "2.4.0-M1"

lazy val magnoliaDependencies =
  libraryDependencies ++= {
    if (scalaVersion.value == Scala3) Seq.empty // Just to make IntelliJ happy
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

lazy val allProjects: Seq[Project] = Seq(
  zioConfig,
  zioConfigAws,
  zioConfigZioAws,
  zioConfigRefined,
  zioConfigPureconfig,
  zioConfigDerivation,
  zioConfigMagnolia,
  zioConfigTypesafe,
  zioConfigTypesafeMagnoliaTests,
  zioConfigXml,
  zioConfigYaml,
  zioConfigScalaz,
  zioConfigCats,
  zioConfigEnumeratum,
  examples
).flatMap(_.componentProjects) ++ Seq(docs)

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(allProjects.map(p => p: ProjectReference): _*)

lazy val zioConfig = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(stdSettings(Some("zio-config")))
  .settings(crossProjectSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings("zio.config"))
  .settings(macroDefinitionSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"                %%% "zio"                     % Versions.zio,
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.12.0",
      "dev.zio"                %%% "zio-test"                % Versions.zio % Test,
      "dev.zio"                %%% "zio-test-sbt"            % Versions.zio % Test
    )
  )
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)

lazy val zioConfigAws = crossProject(JVMPlatform)
  .in(file("aws"))
  .settings(stdSettings(Some("zio-config-aws")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-ssm" % Versions.aws,
      "dev.zio"      %% "zio-streams"      % Versions.zio,
      "dev.zio"      %% "zio-test"         % Versions.zio % Test,
      "dev.zio"      %% "zio-test-sbt"     % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)

lazy val zioConfigZioAws = crossProject(JVMPlatform)
  .in(file("zio-aws"))
  .settings(stdSettings(Some("zio-config-zio-aws")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-aws-ssm"  % Versions.zioAws,
      "dev.zio" %% "zio-streams"  % Versions.zio,
      "dev.zio" %% "zio-test"     % Versions.zio % Test,
      "dev.zio" %% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)

lazy val zioConfigRefined = crossProject(JSPlatform, JVMPlatform, NativePlatform)
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
  .dependsOn(zioConfigMagnolia)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)

lazy val zioConfigPureconfig = crossProject(JVMPlatform)
  .in(file("pureconfig"))
  .settings(stdSettings(Some("zio-config-pureconfig")))
  .settings(crossProjectSettings)
  .settings(
    Dependencies.pureconfig,
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-test"     % Versions.zio % Test,
        "dev.zio" %% "zio-test-sbt" % Versions.zio % Test
      )
  )
  .dependsOn(zioConfig % "test->test", zioConfigTypesafe)
  .jvmSettings(jvmSettings)

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = crossProject(JVMPlatform)
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
              runs.toTask(s" ${cc}").value
            }
          }

          Def.sequential(runTasks)
        })
        .value
  )
  .dependsOn(zioConfig, zioConfigMagnolia, zioConfigRefined, zioConfigTypesafe, zioConfigYaml)
  .jvmSettings(jvmSettings)

lazy val zioConfigDerivation = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("derivation"))
  .settings(stdSettings(Some("zio-config-derivation")))
  .settings(crossProjectSettings)
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
// FIXME: annotations for Scala 3 are not implemented, tests in zioConfigTypesafeMagnoliaTests fail

lazy val zioConfigMagnolia = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("magnolia"))
  .settings(stdSettings(Some("zio-config-magnolia")))
  .settings(crossProjectSettings)
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
  .dependsOn(zioConfig, zioConfigDerivation)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(
    nativeSettings ++ Seq(
      crossScalaVersions := Seq(Scala3)
    )
  )

lazy val zioConfigTypesafe = crossProject(JVMPlatform)
  .in(file("typesafe"))
  .settings(stdSettings(Some("zio-config-typesafe")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.3",
      "dev.zio"     %% "zio-test"     % Versions.zio % Test,
      "dev.zio"     %% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)

lazy val zioConfigYaml = crossProject(JVMPlatform)
  .in(file("yaml"))
  .settings(stdSettings(Some("zio-config-yaml")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.snakeyaml" % "snakeyaml-engine" % "2.8",
      "dev.zio"      %% "zio-test"         % Versions.zio % Test,
      "dev.zio"      %% "zio-test-sbt"     % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)

lazy val zioConfigXml = crossProject(JSPlatform, JVMPlatform) // Note: zio-parser is not available for Scala Native 0.5
  .in(file("xml"))
  .settings(stdSettings(Some("zio-config-xml")))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-parser"   % "0.1.10",
      "dev.zio" %%% "zio-test"     % Versions.zio % Test,
      "dev.zio" %%% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)

lazy val zioConfigScalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalaz"))
  .settings(stdSettings(Some("zio-config-scalaz")))
  .settings(crossProjectSettings)
  .settings(crossScalaVersions := Seq(Scala213, Scala3))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core"  % "7.4.0-M15",
      "dev.zio"    %%% "zio-test"     % Versions.zio % Test,
      "dev.zio"    %%% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)

lazy val zioConfigCats = crossProject(JSPlatform, JVMPlatform, NativePlatform)
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
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)

lazy val zioConfigEnumeratum = crossProject(JSPlatform, JVMPlatform, NativePlatform)
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
  .dependsOn(zioConfig)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)

lazy val zioConfigTypesafeMagnoliaTests = crossProject(JVMPlatform)
  .in(file("typesafe-magnolia-tests"))
  .settings(stdSettings(Some("zio-config-typesafe-magnolia-tests")))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions := Seq(Scala212, Scala213)
  ) // FIXME: annotations for Scala 3 are not implemented, tests fail
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.3",
      "dev.zio"     %% "zio-test"     % Versions.zio % Test,
      "dev.zio"     %% "zio-test-sbt" % Versions.zio % Test
    )
  )
  .dependsOn(zioConfig, zioConfigTypesafe, zioConfigMagnolia, zioConfigDerivation)
  .jvmSettings(jvmSettings)

lazy val docs = project
  .in(file("zio-config-docs"))
  .settings(
    moduleName                                 := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    Dependencies.magnolia,
    Dependencies.refined,
    projectName                                := "ZIO Config",
    mainModuleName                             := (zioConfig.jvm / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(
        zioConfig.jvm,
        zioConfigTypesafe.jvm,
        zioConfigDerivation.jvm,
        zioConfigYaml.jvm,
        zioConfigRefined.jvm
      )
  )
  .settings(macroDefinitionSettings)
  .dependsOn(
    zioConfig.jvm,
    zioConfigTypesafe.jvm,
    zioConfigDerivation.jvm,
    zioConfigYaml.jvm,
    zioConfigRefined.jvm,
    zioConfigMagnolia.jvm
  )
  .enablePlugins(WebsitePlugin)
