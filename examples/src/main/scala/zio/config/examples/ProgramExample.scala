package zio.config.examples

import zio.{ App, UIO, ZIO }
import zio.config.{ Config, _ }
import Config._

/**
 * The pattern is an inspiration from http://degoes.net/articles/zio-environment.
 * We will see how using zio-config library gels well with this pattern.
 * The  functions of the `Application` are simply zio values with this pattern.
 */
final case class ProgramConfig(inputPath: String, outputPath: String)

object ProgramExample extends App {

  private val programConfig =
    (string("INPUT_PATH") <*> string("OUTPUT_PATH"))(ProgramConfig.apply, ProgramConfig.unapply)

  case class Live(config: Config.Service[ProgramConfig], spark: SparkEnv.Service)
      extends SparkEnv
      with Config[ProgramConfig]

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    val pgm = for {
      confService <- Config.fromEnv(programConfig)
      session     <- ZIO.effect("sparkSession")
      _           <- Application.execute.provide(Live(confService.config, new SparkEnv.Live(session)))
    } yield ()

    pgm.foldM(
      fail => ZIO.effectTotal(println(s"failed $fail")) *> ZIO.succeed(1),
      _ => ZIO.effectTotal(println(s"succeeded")) *> ZIO.succeed(0)
    )

  }
}

// spark service
trait SparkEnv {
  def spark: SparkEnv.Service
}

object SparkEnv {
  type SparkSession = String

  trait Service {
    def getSpark: UIO[SparkSession]
  }

  class Live(r: SparkSession) extends Service {
    def getSpark: UIO[SparkSession] = ZIO.succeed(r)
  }
}

// The core application
object Application {
  val processData: ZIO[SparkEnv with Config[ProgramConfig], Throwable, Unit] =
    ZIO.accessM(env => {
      for {
        conf  <- config[ProgramConfig]
        spark <- env.spark.getSpark
        _     <- ZIO.effect(println(s"Executing ${conf.inputPath} and ${conf.outputPath} using ${spark}"))
      } yield ()
    })

  val logSparkSession: ZIO[SparkEnv, Throwable, Unit] =
    ZIO.accessM(
      r =>
        for {
          session <- r.spark.getSpark
          _ <- ZIO.effect(
                println(s"Executing something with spark $session without the need of anything else from config")
              )
        } yield ()
    )

  val logProgramConfig: ZIO[Config[ProgramConfig], Throwable, Unit] =
    for {
      r <- config[ProgramConfig]
      _ <- ZIO
            .effect(
              println(
                s"Executing something with programConfig's parameters ${r.inputPath} and ${r.outputPath} without the need of sparkSession"
              )
            )
    } yield ()

  val execute: ZIO[SparkEnv with Config[ProgramConfig], Throwable, Unit] =
    for {
      _ <- logSparkSession
      _ <- logProgramConfig
      _ <- processData
    } yield ()
}
