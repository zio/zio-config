package zio.config.examples

import zio.blocking.Blocking
import zio.config.Config._
import zio.config.{ Config, _ }
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
      with Blocking.Live

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    val pgm =
      for {
        configEnv <- Config.fromEnv(programConfig)
        sparkEnv  <- SparkEnv.local("some-app")
        _         <- Application.execute.provide(Live(configEnv.config, sparkEnv.spark))
      } yield ()

    ZIO.accessM[Environment] { env =>
      pgm.foldM(
        fail => env.console.putStrLn(s"failed $fail") *> ZIO.succeed(1),
        _ => env.console.putStrLn(s"succeeded") *> ZIO.succeed(0)
      )
    }
  }
}

//// stubs for the real Spark

trait SparkSession {
  def sql(value: String): DataFrame =
    new DataFrame {}

  def version: String =
    "someVersion"
}

trait DataFrame

////

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
      // SparkSession.builder().appName(name).enableHiveSupport().getOrCreate()
      new SparkSession {}
    }

  def cluster(name: String): ZIO[Blocking, Throwable, SparkEnv] =
    make {
      // SparkSession.builder().appName(name).master("local").getOrCreate()
      new SparkSession {}
    }

}

////

// The core application
object Application {
  val logProgramConfig: ZIO[Config[ProgramConfig], Throwable, Unit] =
    for {
      r <- config[ProgramConfig]
      _ <- ZIO.effect(
            println(
              s"Executing something with programConfig's parameters ${r.inputPath} and ${r.outputPath} without the need of sparkSession"
            )
          )
    } yield ()

  val runSparkJob: ZIO[SparkEnv with Blocking, Throwable, Unit] =
    for {
      session <- ZIO.accessM[SparkEnv](_.spark.sparkEnv)
      result  <- zio.blocking.effectBlocking(session.sql("SELECT something"))
      _       <- ZIO.effect(println(s"Executing something with spark ${session.version}: ${result}"))
    } yield ()

  val processData: ZIO[SparkEnv with Config[ProgramConfig], Throwable, Unit] =
    for {
      conf  <- config[ProgramConfig]
      spark <- ZIO.access[SparkEnv](_.spark)
      _     <- ZIO.effect(println(s"Executing ${conf.inputPath} and ${conf.outputPath} using ${spark}"))
    } yield ()

  val execute: ZIO[SparkEnv with Config[ProgramConfig] with Blocking, Throwable, Unit] =
    for {
      _ <- logProgramConfig
        _ <- runSparkJob
        _ <- processData
    } yield ()
}
