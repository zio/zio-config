import explicitdeps.ExplicitDepsPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._
import sbtbuildinfo.BuildInfoKeys._
import sbtbuildinfo._
import scalafix.sbt.ScalafixPlugin.autoImport._
import sbtprojectmatrix.ProjectMatrixKeys.virtualAxes

object BuildHelper {

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    s"-release:${Versions.JdkReleaseVersion}"
  ) ++
    Seq("-Xfatal-warnings")

  private val std2xOptions = Seq(
    "-language:higherKinds",
    "-language:existentials",
    "-explaintypes",
    "-Yrangepos",
    "-Xlint:_,-missing-interpolator,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )

  private def optimizerOptions(optimize: Boolean) =
    if (optimize)
      Seq(
        "-opt:l:inline",
        "-opt-inline-from:zio.internal.**"
      )
    else Nil

  def buildInfoSettings(packageName: String) =
    Seq(
      buildInfoKeys    := Seq[BuildInfoKey](organization, moduleName, name, version, scalaVersion, sbtVersion, isSnapshot),
      buildInfoPackage := packageName
    )

  // Keep this consistent with the version in .core-tests/shared/src/test/scala/REPLSpec.scala
  val replSettings = makeReplSettings {
    """|import zio._
       |import zio.console._
       |import zio.duration._
       |import zio.Runtime.default._
       |implicit class RunSyntax[A](io: ZIO[ZEnv, Any, A]){ def unsafeRun: A = Runtime.default.unsafeRun(io.provideLayer(ZEnv.live)) }
    """.stripMargin
  }

  // Keep this consistent with the version in .streams-tests/shared/src/test/scala/StreamREPLSpec.scala
  val streamReplSettings = makeReplSettings {
    """|import zio._
       |import zio.console._
       |import zio.duration._
       |import zio.stream._
       |import zio.Runtime.default._
       |implicit class RunSyntax[A](io: ZIO[ZEnv, Any, A]){ def unsafeRun: A = Runtime.default.unsafeRun(io.provideLayer(ZEnv.live)) }
    """.stripMargin
  }

  def makeReplSettings(initialCommandsStr: String) = Seq(
    // In the repl most warnings are useless or worse.
    // This is intentionally := as it's more direct to enumerate the few
    // options we do want than to try to subtract off the ones we don't.
    // One of -Ydelambdafy:inline or -Yrepl-class-based must be given to
    // avoid deadlocking on parallel operations, see
    //   https://issues.scala-lang.org/browse/SI-9076
    Compile / console / scalacOptions   := Seq(
      "-Ypartial-unification",
      "-language:higherKinds",
      "-language:existentials",
      "-Yno-adapted-args",
      "-Xsource:2.13",
      "-Yrepl-class-based"
    ),
    Compile / console / initialCommands := initialCommandsStr
  )

  def extraOptions(scalaVersion: String, optimize: Boolean) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _))  =>
        Seq(
          "-language:implicitConversions",
          "-Xignore-scala2-macros",
          "-Xmax-inlines:64"
        )
      case Some((2, 13)) =>
        Seq(
          "-Wunused:params,-implicits,-nowarn"
        ) ++ std2xOptions ++ optimizerOptions(optimize)
      case Some((2, 12)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:_,imports",
          "-Ywarn-unused:imports",
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Ywarn-unused:params,-implicits",
          "-Xfuture",
          "-Xsource:2.13",
          "-Xmax-classfile-name",
          "242"
        ) ++ std2xOptions ++ optimizerOptions(optimize)
      case _             => Seq.empty
    }

  lazy val crossProjectSettings = Seq(
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    scalacOptions --= {
      if (scalaVersion.value == Versions.Scala3)
        Seq("-Xfatal-warnings")
      else
        Seq("-Xprint:typer")
    }
  )

  def stdSettings(prjName: String) = Seq(
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    name                     := s"$prjName",
    scalacOptions ++= stdOptions ++ extraOptions(scalaVersion.value, optimize = !isSnapshot.value),
    libraryDependencies ++= {
      if (scalaVersion.value == Versions.Scala3)
        Seq.empty
      else
        Seq(
          compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full)
        )
    },
    semanticdbEnabled        := scalaVersion.value != Versions.Scala3, // enable SemanticDB
    semanticdbOptions += "-P:semanticdb:synthetics:on",
    semanticdbVersion        := scalafixSemanticdb.revision,           // use Scalafix compatible version
    ThisBuild / scalafixDependencies ++= List(
      "com.github.liancheng" %% "organize-imports" % "0.6.0",
      "com.github.vovapolu"  %% "scaluzzi"         % "0.1.23"
    ),
    Test / parallelExecution := true,
    incOptions ~= (_.withLogRecompileOnMacro(false)),
    autoAPIMappings          := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )

  def macroExpansionSettings = Seq(
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-Ymacro-annotations")
        case _             => Seq.empty
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, x)) if x <= 12 =>
          Seq(compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)))
        case _                       => Seq.empty
      }
    }
  )

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= {
      if (scalaVersion.value == Versions.Scala3) Seq()
      else
        Seq(
          "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided",
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
        )
    }
  )

  lazy val jsSettings = Seq()

  lazy val jvmSettings = Seq()

  lazy val nativeSettings = Seq(
    doc / skip              := true,
    Compile / doc / sources := Seq.empty
  )

  def welcomeMessage = onLoadMessage := {
    import scala.Console

    def header(text: String): String = s"${Console.RED}$text${Console.RESET}"

    def item(text: String): String    = s"${Console.GREEN}> ${Console.CYAN}$text${Console.RESET}"
    def subItem(text: String): String = s"  ${Console.YELLOW}> ${Console.CYAN}$text${Console.RESET}"

    s"""|${header(" ________ ___")}
        |${header("|__  /_ _/ _ \\")}
        |${header("  / / | | | | |")}
        |${header(" / /_ | | |_| |")}
        |${header(s"/____|___\\___/   ${version.value}")}
        |
        |Useful sbt tasks:
        |${item("compileAll")} - Prepares sources & compiles.
        |${item("testAll")} - Prepares sources & runs all tests.
        |${item("fix")} - Fixes sources files using scalafix
        |${item("fmt")} - Formats source files using scalafmt
        |${item("testJVM")} - Runs all JVM tests
        |${item("testJS")} - Runs all ScalaJS tests
        |${item("testNative")} - Runs all Scala Native tests
        |${item("testOnly *.YourSpec -- -t \"YourLabel\"")} - Only runs tests with matching term e.g.
        |${item("docs/docusaurusCreateSite")} - Generates the ZIO microsite
      """.stripMargin
  }

  implicit class ModuleHelper(p: Project) {
    def module: Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }
}
