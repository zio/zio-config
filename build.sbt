import BuildHelper._

welcomeMessage

ThisBuild / scalaVersion := Scala213

inThisBuild(
  List(
    organization := "dev.zio",
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
    )
  )
)

addCommandAlias("fmt", "; scalafmtSbt; scalafmt; test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("compileAll", "; root2-12/compile; root2-13/compile; root3/compile;")
addCommandAlias("testAll", "; root2-12/test; root2-13/test; root3/test;")
addCommandAlias(
  "testJS",
  ";" + selectProjects(VirtualAxis.js).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testJS212",
  ";" + selectProjects(Scala212, VirtualAxis.js).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testJS213",
  ";" + selectProjects(Scala213, VirtualAxis.js).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testJS3",
  ";" + selectProjects(Scala3, VirtualAxis.js).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testJVM",
  ";" + selectProjects(VirtualAxis.jvm).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testJVM212",
  ";" + selectProjects(Scala212, VirtualAxis.jvm).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testJVM213",
  ";" + selectProjects(Scala213, VirtualAxis.jvm).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testJVM3",
  ";" + selectProjects(Scala3, VirtualAxis.jvm).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testNative",
  ";" + selectProjects(VirtualAxis.native).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testNative212",
  ";" + selectProjects(Scala212, VirtualAxis.native).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testNative213",
  ";" + selectProjects(Scala213, VirtualAxis.native).map(_.id + "/test;").mkString
)
addCommandAlias(
  "testNative3",
  ";" + selectProjects(Scala3, VirtualAxis.native).map(_.id + "/test;").mkString
)

val awsVersion        = "1.12.773"
val zioAwsVersion     = "7.21.15.15"
val zioVersion        = "2.1.9"
val magnoliaVersion   = "0.17.0"
val refinedVersion    = "0.11.2"
val pureconfigVersion = "0.17.7"

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

lazy val allProjects = Seq[sbt.internal.ProjectMatrix](
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
  examples,
  docs
)

def selectProjects(scalaVersion: String) = 
  allProjects.flatMap(_.filterProjects(Seq(VirtualAxis.scalaVersionAxis(scalaVersion, ""))))

def selectProjects(platform: VirtualAxis.PlatformAxis) = 
  allProjects.flatMap(_.filterProjects(Seq(platform)))

def selectProjects(scalaVersion: String, platform: VirtualAxis.PlatformAxis) = 
  allProjects.flatMap(_.filterProjects(Seq(platform, VirtualAxis.scalaVersionAxis(scalaVersion, ""))))

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(allProjects.flatMap(_.projectRefs): _*)

lazy val `root2-12` =
  project
    .in(file("2-12"))
    .settings(publish / skip := true)
    .aggregate(selectProjects(Scala212).map(_.project): _*)

lazy val `root2-13` =
  project
    .in(file("2-13"))
    .settings(publish / skip := true)
    .aggregate(selectProjects(Scala213).map(_.project): _*)

lazy val `root3` =
  project
    .in(file("3"))
    .settings(publish / skip := true)
    .aggregate(selectProjects(Scala3).map(_.project): _*)

lazy val zioConfig = projectMatrix
  .in(file("core"))
  .settings(stdSettings("zio-config"))
  .settings(crossProjectSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings("zio.config"))
  .settings(macroDefinitionSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"                %%% "zio"                     % zioVersion,
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.12.0",
      "dev.zio"                %%% "zio-test"                % zioVersion % Test,
      "dev.zio"                %%% "zio-test-sbt"            % zioVersion % Test
    )
  )
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))
  .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3), settings = nativeSettings)

lazy val zioConfigAws = projectMatrix
  .in(file("aws"))
  .settings(stdSettings("zio-config-aws"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-ssm" % awsVersion,
      "dev.zio"      %% "zio-streams"      % zioVersion,
      "dev.zio"      %% "zio-test"         % zioVersion % Test,
      "dev.zio"      %% "zio-test-sbt"     % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigZioAws = projectMatrix
  .in(file("zio-aws"))
  .settings(stdSettings("zio-config-zio-aws"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-aws-ssm"  % zioAwsVersion,
      "dev.zio" %% "zio-streams"  % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigZioAwsJVM = zioConfigZioAws.jvm

lazy val zioConfigRefined = projectMatrix
  .in(file("refined"))
  .settings(stdSettings("zio-config-refined"))
  .settings(crossProjectSettings)
  .settings(
    refinedDependencies,
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      )
  )
  .dependsOn(zioConfigMagnolia % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigPureconfig = projectMatrix
  .in(file("pureconfig"))
  .settings(stdSettings("zio-config-pureconfig"))
  .settings(crossProjectSettings)
  .settings(dottySettings)
  .settings(
    pureconfigDependencies,
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      )
  )
  .dependsOn(zioConfig % "test->test", zioConfigTypesafe)
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = projectMatrix
  .in(file("examples"))
  .settings(stdSettings("zio-config-examples"))
  .settings(crossProjectSettings)
  .settings(
    publish / skip := true,
    fork           := true,
    magnoliaDependencies,
    refinedDependencies,
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
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigDerivation = projectMatrix
  .in(file("derivation"))
  .settings(stdSettings("zio-config-derivation"))
  .settings(crossProjectSettings)
  .dependsOn(zioConfig)
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

// FIXME: annotations for Scala 3 are not implemented, tests in zioConfigTypesafeMagnoliaTests fail
lazy val zioConfigMagnolia = projectMatrix
  .in(file("magnolia"))
  .settings(stdSettings("zio-config-magnolia"))
  .settings(crossProjectSettings)
  .settings(
    magnoliaDependencies,
    scalacOptions ++= {
      if (scalaVersion.value == Scala3)
        Seq.empty
      else
        Seq("-language:experimental.macros")
    },
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigDerivation)
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigTypesafe = projectMatrix
  .in(file("typesafe"))
  .settings(stdSettings("zio-config-typesafe"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.3",
      "dev.zio"     %% "zio-test"     % zioVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigYaml = projectMatrix
  .in(file("yaml"))
  .settings(stdSettings("zio-config-yaml"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.snakeyaml" % "snakeyaml-engine" % "2.8",
      "dev.zio"      %% "zio-test"         % zioVersion % Test,
      "dev.zio"      %% "zio-test-sbt"     % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigXml = projectMatrix
  .in(file("xml"))
  .settings(stdSettings("zio-config-xml"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-parser"   % "0.1.10",
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigScalaz = projectMatrix
  .in(file("scalaz"))
  .settings(stdSettings("zio-config-scalaz"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core"  % "7.4.0-M15",
      "dev.zio"    %%% "zio-test"     % zioVersion % Test,
      "dev.zio"    %%% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala213, Scala3))
  .jsPlatform(scalaVersions = Seq(Scala213, Scala3))
  .nativePlatform(scalaVersions = Seq(Scala213, Scala3))

lazy val zioConfigCats = projectMatrix
  .in(file("cats"))
  .settings(stdSettings("zio-config-cats"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"    % "2.12.0",
      "dev.zio"       %%% "zio-test"     % zioVersion % Test,
      "dev.zio"       %%% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))
  .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigEnumeratum = projectMatrix
  .in(file("enumeratum"))
  .settings(stdSettings("zio-config-enumeratum"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum"   % "1.7.5",
      "dev.zio"      %%% "zio-test"     % zioVersion % Test,
      "dev.zio"      %%% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")
  .jvmPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))
  .jsPlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))
  .nativePlatform(scalaVersions = Seq(Scala212, Scala213, Scala3))

lazy val zioConfigEnumeratumJVM = zioConfigEnumeratum.jvm

lazy val zioConfigTypesafeMagnoliaTests = projectMatrix
  .in(file("typesafe-magnolia-tests"))
  .settings(stdSettings("zio-config-typesafe-magnolia-tests"))
  .settings(crossProjectSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.3",
      "dev.zio"     %% "zio-test"     % zioVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigTypesafe, zioConfigMagnolia, zioConfigDerivation)
  .jvmPlatform(scalaVersions =
    Seq(Scala212, Scala213 /*, Scala3*/ )
  ) // FIXME: annotations for Scala 3 are not implemented, tests fail

lazy val docs = projectMatrix
  .in(file("zio-config-docs"))
  .settings(
    moduleName                                 := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    magnoliaDependencies,
    refinedDependencies,
    projectName                                := "ZIO Config",
    mainModuleName                             := (zioConfig.jvm(Scala213) / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(
        Seq(zioConfig, zioConfigTypesafe, zioConfigDerivation, zioConfigYaml, zioConfigRefined, zioConfigMagnolia)
          .flatMap(_.filterProjects(Seq(VirtualAxis.jvm, VirtualAxis.scalaVersionAxis(Scala213, ""))))
          .map(_.project): _*
      )
  )
  .settings(macroDefinitionSettings)
  .jvmPlatform(scalaVersions = Seq(Scala213))
  .dependsOn(
    zioConfig,
    zioConfigTypesafe,
    zioConfigDerivation,
    zioConfigYaml,
    zioConfigRefined,
    zioConfigMagnolia
  )
  .enablePlugins(WebsitePlugin)
