package zio.config.examples

import zio.{ App, Task, ZIO }
import zio.config._
import client._
import bootstrap._

/**
 * The pattern is a partial inspiration from http://degoes.net/articles/zio-environment, and is just an example.
 *
 * A program consist of a boostrap set up and then an intersection of clients / environment used in different parts of
 * the app.
 *
 * Refer to {{{object bootstrap}}} and {{{object client}}}.
 *
 * Many times clients are derived form of bootstrapping. But a client could be a standalone one formed elsewhere as
 * well.
 *
 * Bootstrapping examples:
 * Loading the {{{ConfigSource}}}, and it could be sys env, property files etc.
 * Loading a spark session {{{SparEnv}}} that shouldn't be recomputed.
 *
 * The clients are simple traits, to form an intersection type.
 * {{{ Example: client.Spark with client.ProgramConfig with client.Xyz }}}.
 *
 * In this way, we made the full use of reader monad pattern,
 * and the variance semantics of ZIO to overcome scala type system limitations, without non-obvious recomputations.
  **/
final case class ProgramConfig(inputPath: String, outputPath: String)

object ProgramExample extends App {
  private val config =
    (string("INPUT_PATH") |@| string("OUTPUT_PATH"))(ProgramConfig.apply, ProgramConfig.unapply)

  // The use of explicit bootstrap instead of (naive approach of forming a ZIO[Any, Throwable, Unit]) is that
  // it allows you to give a test Bootstrap Service. Otherwise you will end up mocking/passing a much more lower level
  // types with in the application such as the derived `ProgramConfig`.
  // This applies for the spark session service as well.
  val mainAppLogic: ZIO[Bootstrap, Throwable, Unit] =
    ZIO.accessM { env =>
      for {
        result                  <- read(config).run.mapError(t => new RuntimeException(s"Failed to parse config: $t"))
        (report, programConfig) = result
        _                       <- ZIO.effect(println(report))
        _                       <- env.spark.getSpark.flatMap(s => Application.execute.provide(Client(s, programConfig)))
      } yield ()
    }

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    val pgm = for {
      env <- envSource
      spark = new SparkEnv.Service {
        override def getSpark: Task[SparkSession] = ZIO.effect("sparkSession")
      }
      _ <- mainAppLogic.provide(Bootstrap(env.configService, spark))
    } yield ()

    pgm.foldM(
      fail => ZIO.effectTotal(println(s"failed $fail")) *> ZIO.succeed(1),
      _ => ZIO.effectTotal(println(s"succeeded")) *> ZIO.succeed(0)
    )

  }
}

// bootstrap modules
object bootstrap {
  type SparkSession = String

  trait SparkEnv {
    def spark: SparkEnv.Service
  }

  object SparkEnv {
    type SparkSession = String

    trait Service {
      def getSpark: Task[SparkSession]
    }
  }

  case class Bootstrap(configService: ConfigSource.Service, spark: SparkEnv.Service) extends SparkEnv with ConfigSource
}

// client modules
object client {

  // Let sparksession be string for example simplicity.
  type SparkSession = String

  case class Client(sparkSession: SparkSession, config: ProgramConfig) extends Spark with Config

  trait Spark {
    val sparkSession: SparkSession
  }

  trait Config {
    val config: ProgramConfig
  }
}

// The core application
object Application {
  val processData: ZIO[client.Spark with client.Config, Throwable, Unit] =
    ZIO.accessM(
      r => ZIO.effect(println(s"Executing ${r.config.inputPath} and ${r.config.outputPath} using ${r.sparkSession}"))
    )

  val logSparkSession: ZIO[client.Spark, Throwable, Unit] =
    ZIO.accessM(
      r =>
        ZIO.effect(
          println(s"Executing something with spark ${r.sparkSession} without the need of anything else from config")
        )
    )

  val logProgramConfig: ZIO[client.Config, Throwable, Unit] =
    ZIO.accessM(
      r =>
        ZIO
          .effect(
            println(
              s"Executing something with programConfig's parameters ${r.config.inputPath} and ${r.config.outputPath} without the need of sparkSession"
            )
          )
    )

  val execute: ZIO[client.Spark with client.Config, Throwable, Unit] =
    for {
      _ <- logSparkSession
      _ <- logProgramConfig
      _ <- processData
    } yield ()
}
