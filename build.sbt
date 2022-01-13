import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-config/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
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
    )
  )
)

lazy val createProductBuilder = taskKey[Unit]("Generate code for ProductBuilder.scala")

createProductBuilder := {
  val productBuilderFile =
    (zioConfigJVM / sourceDirectory).value / "main" / "scala" / "zio" / "config" / "ProductBuilder.scala"
  val resource           = (Compile / resourceManaged).value / "scalaFmt" / "temporary"
  val scalaFmt           = baseDirectory.value / ".scalafmt.conf"

  ProductBuilderCodeGen.replaceFileSection(
    productBuilderFile,
    "productbuilder",
    ProductBuilderCodeGen.productBuilderCodes :+ "",
    resource,
    scalaFmt
  )
}

addCommandAlias("fmt", "; scalafmtSbt; scalafmt; test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")
addCommandAlias(
  "checkAll",
  "; ++2.11.12; project root2-11; check; ++2.12.13; project root2-12; check; ++2.13.5; project root2-13; check"
)
addCommandAlias("compileAll", "; ++2.11.12; root2-11/compile; ++2.12.13; root2-12/compile; ++2.13.5!; root2-13/compile")
addCommandAlias("testAll", "; ++2.11.12; root2-11/test; ++2.12.13; root2-12/test; ++2.13.5!; root2-13/test")
addCommandAlias(
  "testJS",
  ";zioConfigJS/test"
)
addCommandAlias(
  "testJVM",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigShapelessJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigGenJVM/test;zioConfigRefinedJVM/test;zioConfigMagnoliaJVM/test;examplesJVM/test;zioConfigTypesafeMagnoliaTestsJVM/test;zioConfigAwsJVM/test"
)
addCommandAlias(
  "testNative",
  ";zioConfigNative/compile"
)
addCommandAlias(
  "testJS211",
  ";zioConfigJS/test"
)
addCommandAlias(
  "testJVM211",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigShapelessJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigAwsJVM/test"
)
addCommandAlias(
  "testNative211",
  ";zioConfigNative/compile"
)
addCommandAlias(
  "testDotty",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigAwsJVM/test"
)

lazy val awsVersion        = "1.12.137"
lazy val zioVersion        = "2.0.0-RC1"
lazy val magnoliaVersion   = "0.17.0"
lazy val refinedVersion    = "0.9.28"
lazy val pureconfigVersion = "0.16.0"
lazy val shapelessVersion  = "2.4.0-M1"

lazy val magnoliaDependencies =
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.11" || scalaVersion.value == ScalaDotty) Seq.empty // Just to make IntelliJ happy
    else {
      Seq(
        "com.propensive" %% "magnolia"      % magnoliaVersion,
        "org.scala-lang"  % "scala-reflect" % scalaVersion.value
      )
    }
  }

lazy val refinedDependencies =
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.11" || scalaVersion.value == ScalaDotty) Seq.empty // Just to make IntelliJ happy
    else Seq("eu.timepit" %% "refined" % refinedVersion)
  }

lazy val pureconfigDependencies =
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.11" || scalaVersion.value == ScalaDotty) Seq.empty // Just to make IntelliJ happy
    else Seq("com.github.pureconfig" %% "pureconfig" % pureconfigVersion)
  }

lazy val scala211projects =
  Seq[ProjectReference](
    zioConfigJS,
    zioConfigJVM,
    zioConfigAwsJVM,
    zioConfigNative,
    zioConfigTypesafeJVM,
    zioConfigShapelessJVM,
    zioConfigDerivationJVM,
    zioConfigYamlJVM
  )
lazy val scala212projects = scala211projects ++ Seq[ProjectReference](
  zioConfigGenJVM,
  zioConfigEnumeratumJVM,
  zioConfigCatsJVM,
  zioConfigRefinedJVM,
  zioConfigMagnoliaJVM,
  examplesJVM,
  zioConfigTypesafeMagnoliaTestsJVM
)

lazy val scala213projects = scala212projects ++ Seq[ProjectReference](zioConfigScalazJVM)

lazy val scala3projects =
  Seq[ProjectReference](
    zioConfigJVM,
    zioConfigAwsJVM,
    zioConfigCatsJVM,
    zioConfigDerivationJVM,
    zioConfigEnumeratumJVM,
    zioConfigMagnoliaJVM,
    zioConfigScalazJVM,
    zioConfigTypesafeJVM,
    zioConfigYamlJVM
  )

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(scala213projects: _*)

lazy val `root2-11` =
  project
    .in(file("2-11"))
    .settings(publish / skip := true)
    .aggregate(scala211projects: _*)

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
  .settings(stdSettings("zio-config"))
  .settings(crossProjectSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings("zio.config"))
  .settings(macroDefinitionSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"                %% "zio"                     % zioVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0",
      "dev.zio"                %% "zio-test"                % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )

lazy val zioConfigJS     = zioConfig.js
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
lazy val zioConfigJVM    = zioConfig.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
lazy val zioConfigNative = zioConfig.native
  .settings(nativeSettings)

lazy val zioConfigAws    = crossProject(JVMPlatform)
  .in(file("aws"))
  .settings(stdSettings("zio-config-aws"))
  .settings(crossProjectSettings)
  .settings(dottySettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-ssm" % awsVersion,
      "dev.zio"      %% "zio-test"         % zioVersion % Test,
      "dev.zio"      %% "zio-test-sbt"     % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigAwsJVM = zioConfigAws.jvm
  .settings(dottySettings)

lazy val zioConfigRefined    = crossProject(JVMPlatform)
  .in(file("refined"))
  .settings(stdSettings("zio-config-refined"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
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

lazy val zioConfigPureconfig    = crossProject(JVMPlatform)
  .in(file("pureconfig"))
  .settings(stdSettings("zio-config-pureconfig"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    pureconfigDependencies,
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "test->test", zioConfigTypesafe)

lazy val zioConfigPureconfigJVM = zioConfigPureconfig.jvm

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = crossProject(JVMPlatform)
  .in(file("examples"))
  .settings(stdSettings("zio-config-examples"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    publish / skip := true,
    fork := true,
    magnoliaDependencies,
    refinedDependencies,
    runAllExamples :=
      Def
        .taskDyn({
          val c    = (Compile / discoveredMainClasses).value
          val runs = (Compile / runMain)

          val x = c.map { cc =>
            Def.task {
              runs.toTask(s" ${cc}").value
            }
          }

          Def.sequential(x)
        })
        .value
  )
  .dependsOn(zioConfig, zioConfigMagnolia, zioConfigRefined, zioConfigTypesafe, zioConfigGen)

lazy val examplesJVM = examples.jvm

lazy val zioConfigDerivation = crossProject(JVMPlatform)
  .in(file("derivation"))
  .settings(stdSettings("zio-config-derivation"))
  .settings(crossProjectSettings)
  .settings(dottySettings)
  .dependsOn(zioConfig)

lazy val zioConfigDerivationJVM = zioConfigDerivation.jvm
  .settings(dottySettings)

lazy val zioConfigGen = crossProject(JVMPlatform)
  .in(file("gen"))
  .settings(stdSettings("zio-config-gen"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    magnoliaDependencies,
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-test-magnolia" % zioVersion,
      "org.scalatest" %% "scalatest"         % "3.2.9" % Test
    )
  )
  .dependsOn(zioConfigTypesafe, zioConfigMagnolia)

lazy val zioConfigGenJVM = zioConfigGen.jvm

lazy val zioConfigMagnolia    = crossProject(JVMPlatform)
  .in(file("magnolia"))
  .settings(stdSettings("zio-config-magnolia"))
  .settings(crossProjectSettings)
  .settings(dottySettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    magnoliaDependencies,
    scalacOptions ++= {
      if (scalaVersion.value == ScalaDotty) {
        Seq.empty
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

lazy val zioConfigShapeless    = crossProject(JVMPlatform)
  .in(file("shapeless"))
  .settings(stdSettings("zio-config-shapeless"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-test"      % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt"  % zioVersion % Test,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai"   %% "shapeless"     % shapelessVersion
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigDerivation)

lazy val zioConfigShapelessJVM = zioConfigShapeless.jvm

lazy val zioConfigTypesafe    = crossProject(JVMPlatform)
  .in(file("typesafe"))
  .settings(stdSettings("zio-config-typesafe"))
  .settings(crossProjectSettings)
  .settings(dottySettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.1",
      "dev.zio"     %% "zio-test"     % zioVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigTypesafeJVM = zioConfigTypesafe.jvm
  .settings(dottySettings)

lazy val zioConfigYaml    = crossProject(JVMPlatform)
  .in(file("yaml"))
  .settings(stdSettings("zio-config-yaml"))
  .settings(crossProjectSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.snakeyaml" % "snakeyaml-engine" % "2.2.1",
      "dev.zio"      %% "zio-test"         % zioVersion % Test,
      "dev.zio"      %% "zio-test-sbt"     % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigYamlJVM = zioConfigYaml.jvm
  .settings(dottySettings)

lazy val zioConfigScalaz    = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalaz"))
  .settings(stdSettings("zio-config-scalaz"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211, Scala212),
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core"  % "7.4.0-M7",
      "dev.zio"    %% "zio-test"     % zioVersion % Test,
      "dev.zio"    %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigScalazJVM = zioConfigScalaz.jvm
  .settings(dottySettings)

lazy val zioConfigCats    = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("cats"))
  .settings(stdSettings("zio-config-cats"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"    % "2.7.0",
      "dev.zio"       %% "zio-test"     % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigCatsJVM = zioConfigCats.jvm
  .settings(dottySettings)

lazy val zioConfigEnumeratum    = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("enumeratum"))
  .settings(stdSettings("zio-config-enumeratum"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum"   % "1.7.0",
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
    crossScalaVersions --= Seq(Scala211),
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.1",
      "dev.zio"     %% "zio-test"     % zioVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test", zioConfigTypesafe, zioConfigMagnolia)
lazy val zioConfigTypesafeMagnoliaTestsJVM = zioConfigTypesafeMagnoliaTests.jvm

lazy val docs = project
  .in(file("zio-config-docs"))
  .settings(
    publish / skip := true,
    moduleName := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    magnoliaDependencies,
    refinedDependencies,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      zioConfigJVM,
      zioConfigTypesafeJVM,
      zioConfigShapelessJVM,
      zioConfigDerivationJVM,
      zioConfigYamlJVM,
      zioConfigGenJVM,
      zioConfigRefinedJVM,
      zioConfigMagnoliaJVM
    ),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .settings(macroDefinitionSettings)
  .dependsOn(
    zioConfigJVM,
    zioConfigTypesafeJVM,
    zioConfigShapelessJVM,
    zioConfigDerivationJVM,
    zioConfigYamlJVM,
    zioConfigGenJVM,
    zioConfigRefinedJVM,
    zioConfigMagnoliaJVM
  )
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
