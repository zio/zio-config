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
    (zioConfig / sourceDirectory).value / "main" / "scala" / "zio" / "config" / "ProductBuilder.scala"
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
  Seq[ProjectReference](zioConfig, zioConfigTypesafe, zioConfigShapeless, zioConfigDerivation, zioConfigYaml)
lazy val scala212projects = scala211projects ++ Seq[ProjectReference](
  zioConfigGen,
  zioConfigRefined,
  zioConfigMagnolia,
  examples,
  zioConfigTypesafeMagnoliaTests
)
lazy val scala213projects = scala212projects

lazy val scala3projects =
  Seq[ProjectReference](zioConfig, zioConfigTypesafe, zioConfigYaml, zioConfigDerivation)

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

lazy val zioConfig =
  module("zio-config", "core")
    .enablePlugins(BuildInfoPlugin)
    .settings(buildInfoSettings("zio.config"))
    .settings(dottySettings)
    .settings(macroDefinitionSettings)
    .settings(
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      ),
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
    )

lazy val zioConfigRefined =
  module("zio-config-refined", "refined")
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

lazy val runAllExamples   = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = module("zio-config-examples", "examples")
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

lazy val zioConfigDerivation = module("zio-config-derivation", "derivation")
  .settings(dottySettings)
  .dependsOn(zioConfig)

lazy val zioConfigGen = module("zio-config-gen", "gen")
  .settings(
    crossScalaVersions --= Seq(Scala211),
    magnoliaDependencies,
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-test-magnolia" % zioVersion,
      "org.scalatest" %% "scalatest"         % "3.2.8" % Test
    )
  )
  .dependsOn(zioConfigTypesafe, zioConfigMagnolia)

lazy val zioConfigMagnolia                                = module("zio-config-magnolia", "magnolia")
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

lazy val zioConfigShapeless                               = module("zio-config-shapeless", "shapeless")
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

lazy val zioConfigTypesafe                                =
  module("zio-config-typesafe", "typesafe")
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

lazy val zioConfigYaml                                    =
  module("zio-config-yaml", "yaml")
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

lazy val zioConfigTypesafeMagnoliaTests                   =
  module("zio-config-typesafe-magnolia-tests", "typesafe-magnolia-tests")
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

def module(moduleName: String, fileName: String): Project =
  Project(moduleName, file(fileName))
    .settings(stdSettings(moduleName))
    .settings(
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio" % zioVersion
      )
    )

lazy val docs = project
  .in(file("zio-config-docs"))
  .settings(
    publish / skip := true,
    moduleName := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    magnoliaDependencies,
    refinedDependencies,
    libraryDependencies += "dev.zio" %% "zio" % zioVersion
  )
  .dependsOn(zioConfig, zioConfigMagnolia, zioConfigTypesafe, zioConfigRefined, zioConfigGen)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
