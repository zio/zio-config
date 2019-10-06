package zio.config.examples

import zio.blocking.Blocking
import zio.config.Config._
import zio.config.{ Config, _ }
import zio.console.Console
import zio.{ App, UIO, ZIO }

/**
 * The pattern is an inspiration from http://degoes.net/articles/zio-environment.
 * We will see how using zio-config library gels well with this pattern.
 * The  functions of the `Application` are simply zio values with this pattern.
 */
final case class ProgramConfig(inputPath: String, outputPath: String)

object ProgramExample extends App {

  private val programConfig =
    (string("INPUT_PATH") |@| string("OUTPUT_PATH"))(ProgramConfig.apply, ProgramConfig.unapply)

  case class Live(config: Config.Service[ProgramConfig], spark: SparkEnv.Service)
      extends SparkEnv
      with Config[ProgramConfig]
      with Console.Live
      with Blocking.Live

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    val pgm =
      for {
        configEnv <- Config.fromEnv(programConfig)
        sparkEnv  <- SparkEnv.local("some-app")
        _         <- Application.execute.provide(Live(configEnv.config, sparkEnv.spark))
      } yield ()

    pgm.foldM(
      fail => zio.console.putStrLn(s"Failed $fail") *> ZIO.succeed(1),
      _ => zio.console.putStrLn(s"Succeeded") *> ZIO.succeed(0)
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

trait SparkEnv {
  def spark: SparkEnv.Service
}

object SparkEnv {
  trait Service {
    def sparkEnv: UIO[SparkSession]
  }

  def make(session: => SparkSession): ZIO[Blocking, Throwable, SparkEnv] =
    zio.blocking
      .effectBlocking(session)
      .map(
        sparkSession =>
          new SparkEnv {
            override def spark: Service =
              new Service {
                override def sparkEnv: UIO[SparkSession] =
                  ZIO.succeed(sparkSession)
              }
          }
      )

  def local(name: String): ZIO[Blocking, Throwable, SparkEnv] =
    make {
      // As a real-world example:
      //    SparkSession.builder().appName(name).master("local").getOrCreate()
      SparkSession(name)
    }

  def cluster(name: String): ZIO[Blocking, Throwable, SparkEnv] =
    make {
      // As a real-world example:
      //    SparkSession.builder().appName(name).enableHiveSupport().getOrCreate()
      SparkSession(name)
    }

}

////

// The core application
object Application {
  val logProgramConfig: ZIO[Console with Config[ProgramConfig], Nothing, Unit] =
    for {
      r <- config[ProgramConfig]
      _ <- zio.console.putStrLn(s"Executing with parameters ${r.inputPath} and ${r.outputPath} without sparkSession")
    } yield ()

  val runSparkJob: ZIO[SparkEnv with Console with Blocking, Throwable, Unit] =
    for {
      session <- ZIO.accessM[SparkEnv](_.spark.sparkEnv)
      result  <- zio.blocking.effectBlocking(session.slowOp("SELECT something"))
      _       <- zio.console.putStrLn(s"Executed something with spark ${session.version}: $result")
    } yield ()

  val processData: ZIO[SparkEnv with Config[ProgramConfig] with Console, Throwable, Unit] =
    for {
      conf  <- config[ProgramConfig]
      spark <- ZIO.accessM[SparkEnv](_.spark.sparkEnv)
      _     <- zio.console.putStrLn(s"Executing ${conf.inputPath} and ${conf.outputPath} using ${spark.version}")
    } yield ()

  val execute: ZIO[SparkEnv with Config[ProgramConfig] with Console with Blocking, Throwable, Unit] =
    for {
      _ <- logProgramConfig
      _ <- runSparkJob
      _ <- processData
    } yield ()
}
