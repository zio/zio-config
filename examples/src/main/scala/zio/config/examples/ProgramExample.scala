package zio.config.examples

import zio.blocking.Blocking
import zio.config._
import ConfigDescriptor._
import zio.config.examples.aliases._
import zio.console.Console
import zio.{ console, App, ExitCode, Has, ZEnv, ZIO, ZLayer }

/**
 * The pattern is an inspiration from http://degoes.net/articles/zio-environment.
 * We will see how using zio-config library gels well with this pattern.
 * The  functions of the `Application` are simply zio values with this pattern.
 */
final case class ProgramConfig(inputPath: String, outputPath: String)

object ProgramExample extends App {

  private val programConfig =
    (string("INPUT_PATH") |@| string("OUTPUT_PATH"))(ProgramConfig.apply, ProgramConfig.unapply)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val pgm =
      Application.execute.provideLayer(
        ZLayer.requires[Blocking] ++
          ZLayer.requires[Console] ++
          ZConfig.fromMap(Map("INPUT_PATH" -> "input", "OUTPUT_PATH" -> "output"), programConfig, "constant") ++
          SparkEnv.local("some-app")
      )

    pgm.foldM(
      fail => console.putStrLn(s"Failed $fail").as(ExitCode.failure),
      _ => console.putStrLn(s"Succeeded").as(ExitCode.success)
    )
  }
}

final case class SparkSession(name: String) {
  // stubs for the real Spark
  def slowOp(value: String): Unit =
    Thread.sleep(value.length * 100L)

  def version: String =
    "someVersion"
}

object SparkEnv {

  trait Service {
    def sparkEnv: SparkSession
  }

  def make(session: => SparkSession): ZLayer[Blocking, Throwable, Has[Service]] =
    ZLayer.fromFunctionManyM { blocking =>
      blocking.get
        .effectBlocking(session)
        .map { sparkSession =>
          Has(new Service {
            override def sparkEnv: SparkSession = sparkSession
          })
        }
    }

  def local(name: String): ZLayer[Blocking, Throwable, Has[Service]] =
    make {
      // As a real-world example:
      //    SparkSession.builder().appName(name).master("local").getOrCreate()
      SparkSession(name)
    }

  def cluster(name: String): ZLayer[Blocking, Throwable, Has[Service]] =
    make {
      // As a real-world example:
      //    SparkSession.builder().appName(name).enableHiveSupport().getOrCreate()
      SparkSession(name)
    }
}

// The core application
object Application {

  val logProgramConfig: ZIO[Console with ZConfig[ProgramConfig], Nothing, Unit] =
    for {
      r <- getConfig[ProgramConfig]
      _ <- zio.console.putStrLn(s"Executing with parameters ${r.inputPath} and ${r.outputPath} without sparkSession")
    } yield ()

  val runSparkJob: ZIO[SparkEnv with Console with Blocking, Throwable, Unit] =
    for {
      session <- ZIO.access[SparkEnv](_.get.sparkEnv)
      result  <- zio.blocking.effectBlocking(session.slowOp("SELECT something"))
      _       <- zio.console.putStrLn(s"Executed something with spark ${session.version}: $result")
    } yield ()

  val processData: ZIO[SparkEnv with ZConfig[ProgramConfig] with Console, Throwable, Unit] =
    for {
      conf  <- getConfig[ProgramConfig]
      spark <- ZIO.access[SparkEnv](_.get.sparkEnv)
      _     <- zio.console.putStrLn(s"Executing ${conf.inputPath} and ${conf.outputPath} using ${spark.version}")
    } yield ()

  val execute: ZIO[SparkEnv with ZConfig[ProgramConfig] with Console with Blocking, Throwable, Unit] =
    for {
      _ <- logProgramConfig
      _ <- runSparkJob
      _ <- processData
    } yield ()
}

// Type aliases
object aliases {
  type SparkEnv = Has[SparkEnv.Service]
}
