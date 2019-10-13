import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.dev")),
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

lazy val zioVersion = "1.0.0-RC14"

lazy val root =
  project
    .in(file("."))
    .settings(skip in publish := true)
    .aggregate(zioConfig, examples)

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

lazy val examples = module("examples").dependsOn(zioConfig)

def module(moduleName: String): Project =
  Project(moduleName, file(moduleName))
    .settings(stdSettings("zio-config"))
    .settings(
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio" % zioVersion
      )
    )
