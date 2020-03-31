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
  val productBuilderFile = (sourceDirectory in zioConfig).value / "main" / "scala" / "zio" / "config" / "ProductBuilder.scala"
  val resource           = (resourceManaged in Compile).value / "scalaFmt" / "temporary"
  val scalaFmt           = baseDirectory.value / ".scalafmt.conf"

  ProductBuilderCodeGen.replaceFileSection(
    productBuilderFile,
    "productbuilder",
    ProductBuilderCodeGen.productBuilderCodes :+ "",
    resource,
    scalaFmt
  )
}

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val zioVersion      = "1.0.0-RC18-2"
lazy val magnoliaVersion = "0.12.8"
lazy val refinedVersion  = "0.9.13"

lazy val root =
  project
    .in(file("."))
    .settings(skip in publish := true)
    .aggregate(zioConfig, zioConfigMagnolia, examples, zioConfigRefined, zioConfigTypesafe)

lazy val zioConfig =
  module("zio-config", "core")
    .enablePlugins(BuildInfoPlugin)
    .settings(buildInfoSettings)
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
      libraryDependencies ++=
        Seq(
          "eu.timepit" %% "refined"      % refinedVersion,
          "dev.zio"    %% "zio-test"     % zioVersion % Test,
          "dev.zio"    %% "zio-test-sbt" % zioVersion % Test
        ),
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
    )
    .dependsOn(zioConfig % "compile->compile;test->test")

lazy val runAllExamples = taskKey[Unit]("Run all main classes in examples module")

lazy val examples = module("zio-config-examples", "examples")
  .settings(
    skip in publish := true,
    fork := true,
    libraryDependencies ++= Seq(
      "eu.timepit"            %% "refined"    % refinedVersion,
      "com.propensive"        %% "magnolia"   % magnoliaVersion,
      "com.github.pureconfig" %% "pureconfig" % "0.12.3"
    ),
    runAllExamples :=
      Def
        .taskDyn({
          val c    = (discoveredMainClasses in Compile).value
          val runs = (runMain in Compile)

          val x = c.map(cc => {
            Def.task {
              runs.toTask(s" ${cc}").value
            }
          })

          Def.sequential(x)
        })
        .value
  )
  .dependsOn(zioConfig, zioConfigMagnolia, zioConfigRefined, zioConfigTypesafe)

lazy val zioConfigMagnolia = module("zio-config-magnolia", "magnolia")
  .settings(
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia"     % magnoliaVersion,
      "dev.zio"        %% "zio-test"     % zioVersion % Test,
      "dev.zio"        %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .dependsOn(zioConfig % "compile->compile;test->test")

lazy val zioConfigTypesafe =
  module("zio-config-typesafe", "typesafe")
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe" % "config"        % "1.4.0",
        "dev.zio"      %% "zio-test"     % zioVersion % Test,
        "dev.zio"      %% "zio-test-sbt" % zioVersion % Test
      )
    )
    .dependsOn(zioConfig % "compile->compile;test->test", zioConfigMagnolia)

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
    skip in publish := true,
    moduleName := "zio-config-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "eu.timepit"     %% "refined"  % refinedVersion,
      "dev.zio"        %% "zio"      % zioVersion,
      "com.propensive" %% "magnolia" % magnoliaVersion
    )
  )
  .dependsOn(zioConfig, zioConfigMagnolia, zioConfigTypesafe, zioConfigRefined)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
