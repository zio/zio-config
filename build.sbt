import BuildHelper.*
import zio.sbt.ZioSbtCiPlugin.{CacheDependencies, Checkout, Lint, SetupJava, SetupLibuv, SetupSBT}
import zio.sbt.githubactions.{Condition, Job, Step, Strategy}

welcomeMessage

Global / onChangedBuildSource := ReloadOnSourceChanges

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

ThisBuild / ciEnabledBranches := Seq("series/4.x")

// Preserves the exact test matrix the handwritten workflow ran: 3 JDKs x 3 Scala versions x 3
// platforms, dispatching to the existing testJS/testJVM212/213/3x aliases. zio-sbt-ci's built-in
// per-module Scala-version matrix has no platform axis, so it can't express this build's
// JVM/JS/Native cross-project layout on its own.
ThisBuild / ciTestJobs := Seq(
  Job(
    id = "test",
    name = "Test",
    jobTimeout = Some(30),
    strategy = Some(
      Strategy(
        matrix = Map(
          "java"     -> List("17", "21", "25"),
          "scala"    -> List("2.12.x", "2.13.x", "3.x"),
          "platform" -> List("JS", "JVM", "Native")
        ),
        failFast = false
      )
    ),
    steps = Seq(
      Checkout.value,
      SetupJava("${{ matrix.java }}"),
      SetupSBT,
      CacheDependencies,
      Step.SingleStep(
        name = "Run JS tests",
        condition = Some(Condition.Expression("matrix.platform == 'JS' && !startsWith(matrix.scala, '3.')")),
        run = Some("sbt ++${{ matrix.scala }} testJS")
      ),
      Step.SingleStep(
        name = "Run 2.12 JVM tests",
        condition = Some(Condition.Expression("matrix.platform == 'JVM' && startsWith(matrix.scala, '2.12')")),
        run = Some("sbt ++${{ matrix.scala }} testJVM212")
      ),
      Step.SingleStep(
        name = "Run 2.13 JVM tests",
        condition = Some(Condition.Expression("matrix.platform == 'JVM' && startsWith(matrix.scala, '2.13')")),
        run = Some("sbt ++${{ matrix.scala }} testJVM213")
      ),
      Step.SingleStep(
        name = "Run 3.x JVM tests",
        condition = Some(Condition.Expression("matrix.platform == 'JVM' && startsWith(matrix.scala, '3.')")),
        run = Some("sbt ++${{ matrix.scala }} testJVM3x")
      )
    )
  )
)

// The plugin defaults to `+Test/compile` / `+publishLocal` (cross-building every module across
// root's full crossScalaVersions in one sweep). That sweep force-compiles modules like examplesJVM
// and zioConfigTypesafeMagnoliaTestsJVM under Scala 3 even though they're deliberately excluded
// from scala3projects/testJVM3x (they use APIs, e.g. an extension method needing an import that
// only resolves on 2.x, that aren't Scala-3 compatible yet). The per-module `test` job above
// already exercises every supported module/version/platform combination via the curated
// testJS/testJVM2xx/testJVM3x aliases, so `build` only needs a single-version smoke compile/publish.
ThisBuild / ciCheckArtifactsCompilationSteps := Seq(
  Step.SingleStep(
    name = "Check all code compiles",
    run = Some("sbt --no-colors Test/compile")
  )
)
ThisBuild / ciCheckArtifactsBuildSteps       := Seq(
  Step.SingleStep(
    name = "Check artifacts build process",
    run = Some("sbt --no-colors publishLocal")
  )
)

// The old handwritten "lint" job also ran `sbt checkMimaAll` after formatting; append it here so
// binary-compatibility checking isn't lost by switching to the plugin's generated lint job.
ThisBuild / ciLintJobs := Seq(
  Job(
    id = "lint",
    name = "Lint",
    steps = Seq(Checkout.value, SetupLibuv, SetupJava(ciDefaultJavaVersion.value), SetupSBT, CacheDependencies) ++
      ciCheckGithubWorkflowSteps.value ++
      Seq(
        Lint.value,
        Step.SingleStep(name = "Check binary compatibility", run = Some("sbt --no-colors checkMimaAll"))
      )
  )
)

// The Scala versions come from the CI matrix (see BuildHelper.versions), so these aliases must
// not hardcode them: a matrix bump would leave `++3.3` with nothing to select.
addCommandAlias("lint", s"; ++$Scala213; scalafmtSbtCheck; scalafmtCheck; ++$ScalaDotty; scalafmtCheck")
addCommandAlias("fmt", s"; ++$Scala213; scalafmtSbt; scalafmtAll; ++$ScalaDotty; scalafmtAll")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias(
  "compileAll",
  s"; ++$Scala212; root2-12/compile; ++$Scala213!; root2-13/compile; ++$ScalaDotty!; root3/compile;"
)
addCommandAlias(
  "testAll",
  s"; ++$Scala212; root2-12/test; ++$Scala213!; root2-13/test; ++$ScalaDotty!; root3/test;"
)
addCommandAlias(
  "testJS",
  ";zioConfigJS/test"
)
addCommandAlias(
  "testJVM212",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigTomlJVM/test;examplesJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test;zioConfigPureconfigJVM/test"
)
addCommandAlias(
  "testJVM213",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigTomlJVM/test;zioConfigRefinedJVM/test;zioConfigMagnoliaJVM/test;examplesJVM/test;zioConfigTypesafeMagnoliaTestsJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test;zioConfigPureconfigJVM/test"
)
addCommandAlias(
  "testJVM3x",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigTomlJVM/test;zioConfigMagnoliaJVM/test;zioConfigAwsJVM/test;zioConfigZioAwsJVM/test;zioConfigXmlJVM/test;zioConfigPureconfigJVM/test"
)
addCommandAlias(
  "testJVM",
  ";testJVM212;testJVM213;testJVM3x;"
)

addCommandAlias(
  "checkMima",
  "all zioConfigJVM/mimaReportBinaryIssues zioConfigTypesafeJVM/mimaReportBinaryIssues zioConfigDerivationJVM/mimaReportBinaryIssues zioConfigYamlJVM/mimaReportBinaryIssues zioConfigMagnoliaJVM/mimaReportBinaryIssues zioConfigAwsJVM/mimaReportBinaryIssues zioConfigZioAwsJVM/mimaReportBinaryIssues zioConfigXmlJVM/mimaReportBinaryIssues"
)

addCommandAlias(
  "checkMimaAll",
  s"; ++$Scala212; checkMima; ++$Scala213; checkMima; ++$ScalaDotty; checkMima"
)

val awsVersion        = "1.12.797"
val zioAwsVersion     = "7.28.29.19"
val zioVersion        = "2.1.26"
val magnoliaVersion   = "0.17.0"
val refinedVersion    = "0.11.4"
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

lazy val scala212projects = Seq[ProjectReference](
  zioConfigJS,
  zioConfigJVM,
  zioConfigAwsJVM,
  zioConfigNative,
  zioConfigTypesafeJVM,
  zioConfigDerivationJVM,
  zioConfigYamlJVM,
  zioConfigTomlJVM,
  docs,
  zioConfigEnumeratumJVM,
  zioConfigCatsJVM,
  zioConfigRefinedJVM,
  zioConfigMagnoliaJVM,
  zioConfigTypesafeMagnoliaTestsJVM,
  zioConfigZioAwsJVM,
  zioConfigXmlJVM,
  zioConfigPureconfigJVM,
  examplesJVM
)

lazy val scala213projects = scala212projects ++ Seq[ProjectReference](zioConfigScalazJVM)

lazy val scala3projects =
  Seq[ProjectReference](
    zioConfigJS,
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
    zioConfigTomlJVM,
    zioConfigXmlJVM,
    zioConfigPureconfigJVM,
    docs
  )

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(scala213projects *)
    .enablePlugins(ZioSbtCiPlugin)

lazy val `root2-12` =
  project
    .in(file("2-12"))
    .settings(publish / skip := true)
    .aggregate(scala212projects *)

lazy val `root2-13` =
  project
    .in(file("2-13"))
    .settings(publish / skip := true)
    .aggregate(scala213projects *)

lazy val `root3` =
  project
    .in(file("3"))
    .settings(publish / skip := true)
    .aggregate(scala3projects *)

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
      "dev.zio"                %% "zio"                     % zioVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.14.0",
      "dev.zio"                %% "zio-test"                % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )

lazy val zioConfigJS = zioConfig.js
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val zioConfigJVM = zioConfig.jvm
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val zioConfigNative = zioConfig.native
  .settings(nativeSettings)

lazy val zioConfigAws = crossProject(JVMPlatform)
  .in(file("aws"))
  .settings(stdSettings("zio-config-aws"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-ssm" % awsVersion,
      "dev.zio"      %% "zio-streams"      % zioVersion,
      "dev.zio"      %% "zio-test"         % zioVersion % Test,
      "dev.zio"      %% "zio-test-sbt"     % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigAwsJVM = zioConfigAws.jvm

lazy val zioConfigZioAws = crossProject(JVMPlatform)
  .in(file("zio-aws"))
  .settings(stdSettings("zio-config-zio-aws"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-aws-ssm"  % zioAwsVersion,
      "dev.zio" %% "zio-streams"  % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigZioAwsJVM = zioConfigZioAws.jvm

lazy val zioConfigRefined = crossProject(JVMPlatform)
  .in(file("refined"))
  .settings(stdSettings("zio-config-refined"))
  .settings(crossProjectSettings)
  .settings(
    refinedDependencies,
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
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
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigTypesafe)

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

lazy val zioConfigDerivation = crossProject(JVMPlatform)
  .in(file("derivation"))
  .settings(stdSettings("zio-config-derivation"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .dependsOn(zioConfig)

lazy val zioConfigDerivationJVM = zioConfigDerivation.jvm

lazy val zioConfigMagnolia = crossProject(JVMPlatform)
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
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigDerivation)

lazy val zioConfigMagnoliaJVM = zioConfigMagnolia.jvm

lazy val zioConfigTypesafe = crossProject(JVMPlatform)
  .in(file("typesafe"))
  .settings(stdSettings("zio-config-typesafe"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.9",
      "dev.zio"     %% "zio-test"     % zioVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigTypesafeJVM = zioConfigTypesafe.jvm

lazy val zioConfigYaml = crossProject(JVMPlatform)
  .in(file("yaml"))
  .settings(stdSettings("zio-config-yaml"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.snakeyaml" % "snakeyaml-engine" % "3.1.1",
      "dev.zio"      %% "zio-test"         % zioVersion % Test,
      "dev.zio"      %% "zio-test-sbt"     % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigYamlJVM = zioConfigYaml.jvm

lazy val zioConfigToml = crossProject(JVMPlatform)
  .in(file("toml"))
  .settings(stdSettings("zio-config-toml"))
  .settings(crossProjectSettings)
  // MiMa after the first published release of zio-config-toml
  .settings(
    libraryDependencies ++= Seq(
      "org.tomlj" % "tomlj"        % "2.0.1",
      "dev.zio"  %% "zio-test"     % zioVersion % Test,
      "dev.zio"  %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigTomlJVM = zioConfigToml.jvm

lazy val zioConfigXml = crossProject(JVMPlatform)
  .in(file("xml"))
  .settings(stdSettings("zio-config-xml"))
  .settings(crossProjectSettings)
  .settings(enableMimaSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-parser"   % "0.1.11",
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigXmlJVM = zioConfigXml.jvm

lazy val zioConfigScalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalaz"))
  .settings(stdSettings("zio-config-scalaz"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala212),
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core"  % "7.4.0-M17",
      "dev.zio"    %% "zio-test"     % zioVersion % Test,
      "dev.zio"    %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigScalazJVM = zioConfigScalaz.jvm

lazy val zioConfigCats = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("cats"))
  .settings(stdSettings("zio-config-cats"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"    % "2.13.0",
      "dev.zio"       %% "zio-test"     % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigCatsJVM = zioConfigCats.jvm

lazy val zioConfigEnumeratum = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("enumeratum"))
  .settings(stdSettings("zio-config-enumeratum"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum"   % "1.9.8",
      "dev.zio"      %% "zio-test"     % zioVersion % Test,
      "dev.zio"      %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigEnumeratumJVM = zioConfigEnumeratum.jvm

lazy val zioConfigTypesafeMagnoliaTests    = crossProject(JVMPlatform)
  .in(file("typesafe-magnolia-tests"))
  .settings(stdSettings("zio-config-typesafe-magnolia-tests"))
  .settings(crossProjectSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.9",
      "dev.zio"     %% "zio-test"     % zioVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigTypesafe, zioConfigMagnolia, zioConfigDerivation)
lazy val zioConfigTypesafeMagnoliaTestsJVM = zioConfigTypesafeMagnoliaTests.jvm

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
