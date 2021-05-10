import BuildHelper._
import sbtcrossproject.CrossProject

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
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-config/"), "scm:git:git@github.com:zio/zio-config.git")
    )
  )
)

ThisBuild / publishTo := sonatypePublishToBundle.value

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
  "; ++2.11.12; project root2-11; check; ++2.12.11; project root2-12; check; ++2.13.2; project root2-13; check"
)
addCommandAlias("compileAll", "; ++2.11.12; root2-11/compile; ++2.12.11; root2-12/compile; ++2.13.2!; root2-13/compile")
addCommandAlias("testAll", "; ++2.11.12; root2-11/test; ++2.12.11; root2-12/test; ++2.13.2!; root2-13/test")
addCommandAlias(
  "testJVM",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigShapelessJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test;zioConfigGenJVM/test;zioConfigRefinedJVM/test;zioConfigMagnoliaJVM/test;examplesJVM/test;zioConfigTypesafeMagnoliaTestsJVM/test"
)
addCommandAlias(
  "testJVM211",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigShapelessJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test"
)
addCommandAlias(
  "testDotty",
  ";zioConfigJVM/test;zioConfigTypesafeJVM/test;zioConfigDerivationJVM/test;zioConfigYamlJVM/test"
)

lazy val zioVersion       = "1.0.7"
lazy val magnoliaVersion  = "0.17.0"
lazy val refinedVersion   = "0.9.24"
lazy val shapelessVersion = "2.4.0-M1"

lazy val magnoliaDependencies =
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.11") Seq.empty // Just to make IntelliJ happy
    else
      Seq(
        "com.propensive" %% "magnolia"      % magnoliaVersion,
        "org.scala-lang"  % "scala-reflect" % scalaVersion.value
      )
  }

lazy val refinedDependencies =
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.11") Seq.empty // Just to make IntelliJ happy
    else Seq("eu.timepit" %% "refined" % refinedVersion)
  }

lazy val scala211projects =
  Seq[ProjectReference](
    zioConfigJVM,
    zioConfigTypesafeJVM,
    zioConfigShapelessJVM,
    zioConfigDerivationJVM,
    zioConfigYamlJVM
  )
lazy val scala212projects = scala211projects ++ Seq[ProjectReference](
  zioConfigGenJVM,
  zioConfigRefinedJVM,
  zioConfigMagnoliaJVM,
  examplesJVM,
  zioConfigTypesafeMagnoliaTestsJVM
)
lazy val scala213projects = scala212projects

lazy val scala3projects =
  Seq[ProjectReference](zioConfigJVM, zioConfigTypesafeJVM, zioConfigYamlJVM, zioConfigDerivationJVM)

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

lazy val zioConfig    = crossProject(JVMPlatform)
  .in(file("core"))
  .settings(stdSettings("zio-config"))
  .settings(crossProjectSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings("zio.config"))
  .settings(dottySettings)
  .settings(macroDefinitionSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ) ++ (
      if (scalaVersion.value == Scala211 || scalaVersion.value == Scala212)
        List("org.scala-lang.modules" %% "scala-collection-compat" % "2.4.3")
      else
        List()
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
lazy val zioConfigJVM = zioConfig.jvm
  .settings(
    crossScalaVersions ++= Seq(ScalaDotty)
  )

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

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples    = crossProject(JVMPlatform)
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

lazy val zioConfigDerivation    = crossProject(JVMPlatform)
  .in(file("derivation"))
  .settings(stdSettings("zio-config-derivation"))
  .settings(crossProjectSettings)
  .settings(dottySettings)
  .dependsOn(zioConfig)
lazy val zioConfigDerivationJVM = zioConfigDerivation.jvm
  .settings(
    crossScalaVersions ++= Seq(ScalaDotty)
  )

lazy val zioConfigGen    = crossProject(JVMPlatform)
  .in(file("gen"))
  .settings(stdSettings("zio-config-gen"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    magnoliaDependencies,
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-test-magnolia" % zioVersion,
      "org.scalatest" %% "scalatest"         % "3.2.8" % Test
    )
  )
  .dependsOn(zioConfigTypesafe, zioConfigMagnolia)
lazy val zioConfigGenJVM = zioConfigGen.jvm

lazy val zioConfigMagnolia    = crossProject(JVMPlatform)
  .in(file("magnolia"))
  .settings(stdSettings("zio-config-magnolia"))
  .settings(crossProjectSettings)
  .settings(
    crossScalaVersions --= Seq(Scala211),
    magnoliaDependencies,
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
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"       % "1.4.1",
      "dev.zio"     %% "zio-test"     % zioVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .settings(dottySettings)
  .dependsOn(zioConfig % "compile->compile;test->test")
lazy val zioConfigTypesafeJVM = zioConfigTypesafe.jvm
  .settings(
    crossScalaVersions ++= Seq(ScalaDotty)
  )

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
  .settings(dottySettings)
  .dependsOn(zioConfig % "compile->compile;test->test")
lazy val zioConfigYamlJVM = zioConfigYaml.jvm
  .settings(
    crossScalaVersions ++= Seq(ScalaDotty)
  )

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
      zioConfigMagnoliaJVM,
      zioConfigTypesafeJVM,
      zioConfigRefinedJVM,
      zioConfigGenJVM
    ),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(zioConfigJVM, zioConfigMagnoliaJVM, zioConfigTypesafeJVM, zioConfigRefinedJVM, zioConfigGenJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
