import explicitdeps.ExplicitDepsPlugin.autoImport.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.*
import sbtbuildinfo.*
import sbtbuildinfo.BuildInfoKeys.*
import sbtcrossproject.CrossPlugin.autoImport.*
import sbtprojectmatrix.ProjectMatrixKeys.virtualAxes
import scalafix.sbt.ScalafixPlugin.autoImport.*

object BuildHelper {

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

  lazy val jsSettings = Seq()

  lazy val jvmSettings = Seq()

  lazy val nativeSettings = Seq()

  def usefulTasksAndSettings: Def.Initialize[Seq[(String, String)]] = Def.setting {
    Seq(
      "docs/docusaurusCreateSite" -> "Generates the ZIO microsite."
    )
  }

}
