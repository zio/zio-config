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

lazy val zioVersion      = "1.0.0-RC17"
lazy val magnoliaVersion = "0.12.2"

lazy val root =
  project
    .in(file("."))
    .settings(skip in publish := true)
    .aggregate(zioConfig, examples, zioConfigRefined)

lazy val zioConfig =
  module("zio-config")
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
  module("zio-config-refined")
    .settings(
      libraryDependencies ++=
        Seq(
          "eu.timepit" %% "refined" % "0.9.10"
        )
    )
    .dependsOn(zioConfig)

lazy val examples = module("examples")
  .settings(
    skip in publish := true,
    moduleName := "zio-config-examples",
    fork := true,
    crossScalaVersions := Seq("2.13.0", "2.12.10"),
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia" % magnoliaVersion
    )
  )
  .dependsOn(zioConfig, zioConfigMagnolia)

lazy val zioConfigMagnolia = module("zio-config-magnolia")
  .settings(skip in publish := true)
  .settings(
    crossScalaVersions := Seq("2.13.0", "2.12.10"),
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia" % magnoliaVersion
    )
  )
  .dependsOn(zioConfig)

def module(moduleName: String): Project =
  Project(moduleName, file(moduleName))
    .settings(stdSettings("zio-config"))
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
      "dev.zio"        %% "zio"      % zioVersion,
      "com.propensive" %% "magnolia" % magnoliaVersion
    )
  )
  .dependsOn(zioConfig, zioConfigMagnolia)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
