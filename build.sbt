enablePlugins(ZioSbtCiPlugin)

crossScalaVersions := Seq.empty

inThisBuild(
  List(
    name := "ZIO Config",
    crossScalaVersions -= scala211.value,
    ciEnabledBranches := Seq("series/4.x"),
    developers := List(
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
      supportedScalaVersions := Map(
        (zioConfigJS / thisProject).value.id -> (zioConfigJS / crossScalaVersions).value,
        (zioConfigJVM / thisProject).value.id -> (zioConfigJVM / crossScalaVersions).value,
        (zioConfigNative / thisProject).value.id -> (zioConfigNative / crossScalaVersions).value,
        (zioConfigAwsJVM / thisProject).value.id -> (zioConfigAwsJVM / crossScalaVersions).value,
        (zioConfigCatsJVM / thisProject).value.id -> (zioConfigCatsJVM / crossScalaVersions).value,
        (zioConfigDerivationJVM / thisProject).value.id -> (zioConfigDerivationJVM / crossScalaVersions).value,
        (zioConfigEnumeratumJVM / thisProject).value.id -> (zioConfigEnumeratumJVM / crossScalaVersions).value,
        (zioConfigMagnoliaJVM / thisProject).value.id -> (zioConfigMagnoliaJVM / crossScalaVersions).value,
        (zioConfigPureconfigJVM / thisProject).value.id -> (zioConfigPureconfigJVM / crossScalaVersions).value,
        (zioConfigRefinedJVM / thisProject).value.id -> (zioConfigRefinedJVM / crossScalaVersions).value,
        (zioConfigScalazJVM / thisProject).value.id -> (zioConfigScalazJVM / crossScalaVersions).value,
        (zioConfigTypesafeJVM / thisProject).value.id -> (zioConfigTypesafeJVM / crossScalaVersions).value,
        (zioConfigTypesafeMagnoliaTestsJVM / thisProject).value.id -> (zioConfigTypesafeMagnoliaTestsJVM / crossScalaVersions).value,
        (zioConfigYamlJVM / thisProject).value.id -> (zioConfigYamlJVM / crossScalaVersions).value,
        (zioConfigXmlJVM / thisProject).value.id -> (zioConfigXmlJVM / crossScalaVersions).value,
      )
    )
)

addCommandAlias("fmt", "; scalafmtSbt; scalafmt; test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("compileAll", "; ++2.12.16; root2-12/compile; ++2.13.8!; root2-13/compile")
addCommandAlias("testAll", "; ++2.12.16; root2-12/test; ++2.13.8!; root2-13/test")
addCommandAlias(
  "testJS",
  ";zioConfigJS/test"
)

addCommandAlias(
  "testJVM212",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;examplesJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test"
)
addCommandAlias(
  "testJVM213",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigRefinedJVM/test;zioConfigMagnoliaJVM/test;examplesJVM/test;zioConfigTypesafeMagnoliaTestsJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test"
)
addCommandAlias(
  "testJVM3x",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test"
)

val awsVersion        = "1.12.360"
val zioAwsVersion     = "5.19.8.4"
val zioVersion        = "2.0.10"
val magnoliaVersion   = "0.17.0"
val refinedVersion    = "0.10.2"
val pureconfigVersion = "0.16.0"
val shapelessVersion  = "2.4.0-M1"

lazy val magnoliaDependencies =
  libraryDependencies ++= {
    if (scalaVersion.value == scala211.value || scalaVersion.value == scala3.value) Seq.empty // Just to make IntelliJ happy
    else {
      Seq(
        "com.propensive" %% "magnolia"      % magnoliaVersion,
        "org.scala-lang"  % "scala-reflect" % scalaVersion.value
      )
    }
  }

lazy val refinedDependencies =
  libraryDependencies ++= {
    if (scalaVersion.value == scala211.value) Seq.empty // Just to make IntelliJ happy
    else Seq("eu.timepit" %% "refined" % refinedVersion)
  }

lazy val pureconfigDependencies =
  libraryDependencies ++= {
    if (scalaVersion.value == scala211.value || scalaVersion.value == scala3.value) Seq.empty // Just to make IntelliJ happy
    else Seq("com.github.pureconfig" %% "pureconfig" % pureconfigVersion)
  }

lazy val scala212projects = Seq[ProjectReference](
  zioConfigJS,
  zioConfigJVM,
  zioConfigAwsJVM,
  zioConfigNative,
  zioConfigTypesafeJVM,
  zioConfigDerivationJVM,
  zioConfigYamlJVM,
  docs,
  zioConfigEnumeratumJVM,
  zioConfigCatsJVM,
  zioConfigRefinedJVM,
  zioConfigMagnoliaJVM,
  zioConfigZioAwsJVM,
  zioConfigXmlJVM,
  examplesJVM
)

lazy val scala213projects = scala212projects ++ Seq[ProjectReference](zioConfigScalazJVM)

lazy val scala3projects =
  Seq[ProjectReference](
    zioConfigJVM,
    zioConfigAwsJVM,
    zioConfigZioAwsJVM,
    zioConfigCatsJVM,
    zioConfigDerivationJVM,
    zioConfigEnumeratumJVM,
    zioConfigMagnoliaJVM,
    zioConfigRefinedJVM,
    zioConfigScalazJVM,
    zioConfigTypesafeJVM,
    zioConfigYamlJVM,
    zioConfigXmlJVM,
    docs
  )

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(scala213projects: _*)

lazy val `root2-12` =
  project
    .in(file("2-12"))
    .settings(publish / skip := true)
    .aggregate(scala212projects: _*)

lazy val `root2-13` =
  project
    .in(file("2-13"))
    .settings(publish / skip := true)
    .aggregate(scala213projects: _*)

lazy val `root3` =
  project
    .in(file("3"))
    .settings(publish / skip := true)
    .aggregate(scala3projects: _*)

lazy val zioConfig = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(
    stdSettings(
      name = "zio-config",
      packageName = Some("zio.config"),
      enableCrossProject = true
    )
  )
  .settings(macroDefinitionSettings)
  .settings(enableZIO())
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.8.1",
    )
  )

lazy val zioConfigJS     = zioConfig.js.settings(crossScalaVersions -= scala211.value)
lazy val zioConfigJVM    = zioConfig.jvm.settings(scala3Settings)
lazy val zioConfigNative = zioConfig.native.settings(nativeSettings)

lazy val zioConfigAws    = crossProject(JVMPlatform)
  .in(file("aws"))
  .settings(stdSettings("zio-config-aws", enableCrossProject = true))
  .settings(enableZIO(enableStreaming = true))
  .settings(scala3Settings)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-ssm" % awsVersion
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigAwsJVM = zioConfigAws.jvm.settings(scala3Settings)

lazy val zioConfigZioAws    = crossProject(JVMPlatform)
  .in(file("zio-aws"))
  .settings(stdSettings(name = "zio-config-zio-aws", enableCrossProject = true))
  .settings(enableZIO(enableStreaming = true))
  .settings(scala3Settings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-aws-ssm"  % zioAwsVersion,
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigZioAwsJVM = zioConfigZioAws.jvm.settings(scala3Settings)

lazy val zioConfigRefined    = crossProject(JVMPlatform)
  .in(file("refined"))
  .settings(stdSettings(name = "zio-config-refined", enableCrossProject = true))
  .settings(enableZIO())
  .settings(scala3Settings)
  .settings(
    refinedDependencies
  )
  .dependsOn(zioConfigMagnolia % "compile->compile;test->test")

lazy val zioConfigRefinedJVM = zioConfigRefined.jvm

lazy val zioConfigPureconfig    = crossProject(JVMPlatform)
  .in(file("pureconfig"))
  .settings(stdSettings(name = "zio-config-pureconfig", enableCrossProject = true))
  .settings(enableZIO())
  .settings(
    pureconfigDependencies,
  )
  .dependsOn(zioConfig % "test->test", zioConfigTypesafe)

lazy val zioConfigPureconfigJVM =
  zioConfigPureconfig.jvm.settings(
    crossScalaVersions -= scala3.value
  )

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = crossProject(JVMPlatform)
  .in(file("examples"))
  .settings(stdSettings(name = "zio-config-examples", enableCrossProject = true))
  .settings(
    publish / skip := true,
    fork := true,
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

lazy val examplesJVM = examples.jvm

lazy val zioConfigDerivation = crossProject(JVMPlatform)
  .in(file("derivation"))
  .settings(stdSettings(name = "zio-config-derivation", enableCrossProject = true))
  .settings(scala3Settings)
  .dependsOn(zioConfig)

lazy val zioConfigDerivationJVM = zioConfigDerivation.jvm.settings(scala3Settings)

lazy val zioConfigMagnolia    = crossProject(JVMPlatform)
  .in(file("magnolia"))
  .settings(stdSettings(name = "zio-config-magnolia", enableCrossProject = true))
  .settings(enableZIO())
  .settings(scalacOptions := scalacOptions.value.filterNot(_ == "-noindent"))
  .settings(
    magnoliaDependencies,
    scalacOptions ++= {
      if (scalaVersion.value == scala3.value) {
        Seq.empty
      } else {
        Seq("-language:experimental.macros")
      }
    }
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigDerivation)

lazy val zioConfigMagnoliaJVM = zioConfigMagnolia.jvm

lazy val zioConfigTypesafe    = crossProject(JVMPlatform)
  .in(file("typesafe"))
  .settings(stdSettings(name = "zio-config-typesafe", enableCrossProject = true))
  .settings(enableZIO())
  .settings(scala3Settings)
  .settings(
    libraryDependencies ++= Seq( "com.typesafe" % "config"       % "1.4.2")
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigTypesafeJVM = zioConfigTypesafe.jvm
  .settings(scala3Settings)

lazy val zioConfigYaml    = crossProject(JVMPlatform)
  .in(file("yaml"))
  .settings(stdSettings(name = "zio-config-yaml", enableCrossProject = true))
  .settings(enableZIO())
  .settings(
    libraryDependencies ++= Seq(
      "org.snakeyaml" % "snakeyaml-engine" % "2.6",
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigYamlJVM = zioConfigYaml.jvm.settings(scala3Settings)

lazy val zioConfigXml    = crossProject(JVMPlatform)
  .in(file("xml"))
  .settings(stdSettings(name = "zio-config-xml", enableCrossProject = true))
  .settings(enableZIO())
  .settings(
    libraryDependencies ++= Seq( "dev.zio" %% "zio-parser"   % "0.1.8" )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigXmlJVM = zioConfigXml.jvm.settings(scala3Settings)

lazy val zioConfigScalaz    = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalaz"))
  .settings(stdSettings(name = "zio-config-scalaz", enableCrossProject = true))
  .settings(enableZIO())
  .settings(
    crossScalaVersions --= Seq(scala212.value),
    libraryDependencies ++= Seq( "org.scalaz" %% "scalaz-core"  % "7.4.0-M13")
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigScalazJVM = zioConfigScalaz.jvm
  .settings(scala3Settings)

lazy val zioConfigCats    = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("cats"))
  .settings(stdSettings(name = "zio-config-cats", enableCrossProject = true))
  .settings(enableZIO())
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"    % "2.8.0",
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigCatsJVM = zioConfigCats.jvm.settings(scala3Settings)

lazy val zioConfigEnumeratum    = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("enumeratum"))
  .settings(stdSettings(name = "zio-config-enumeratum", enableCrossProject = true))
  .settings(enableZIO())
  .settings(
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum"   % "1.7.2",
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigEnumeratumJVM = zioConfigEnumeratum.jvm

lazy val zioConfigTypesafeMagnoliaTests    = crossProject(JVMPlatform)
  .in(file("typesafe-magnolia-tests"))
  .settings(stdSettings(name = "zio-config-typesafe-magnolia-tests", enableCrossProject = true))
  .settings(enableZIO())
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.2",
    )
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigTypesafe, zioConfigMagnolia)
  
lazy val zioConfigTypesafeMagnoliaTestsJVM = zioConfigTypesafeMagnoliaTests.jvm

lazy val docs = project
  .in(file("zio-config-docs"))
  .settings(
    moduleName := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    magnoliaDependencies,
    refinedDependencies,
    crossScalaVersions := (zioConfigJVM / crossScalaVersions).value,
    projectName := "ZIO Config",
    mainModuleName := (zioConfigJVM / moduleName).value,
    projectStage := ProjectStage.ProductionReady,
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
