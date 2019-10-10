package zio.config.examples

import zio.{ App, ZIO }
import zio.config.{ Config, _ }
import zio.system.System
import zio.console._
import Config._

/**
 * The pattern is an inspiration from http://degoes.net/articles/zio-environment.
 * We will see how using zio-config library gels well with this pattern.
 * The  functions of the `Application` are simply zio values with this pattern.
 */
final case class ProgramConfig2(inputPath: String, outputPath: String)

object Example extends App {

  private val programConfig =
    (string("INPUT_PATH") |@| string("OUTPUT_PATH"))(ProgramConfig2.apply, ProgramConfig2.unapply)

  val program: ZIO[System with Console, Throwable, Unit] =
    for {
      _           <- putStrLn("What is you name?")
      name        <- getStrLn
      _           <- putStrLn(s"Hi $name, let's get your config!")
      confService <- Config.fromEnv(programConfig)
      programConf <- config[ProgramConfig2].provide(confService)
      _           <- putStrLn(s"INPUT_PATH = ${programConf.inputPath}")
      _           <- putStrLn(s"OUTPUT_PATH = ${programConf.outputPath}")
    } yield ()

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    ZIO.accessM[Environment](env => {
      program.foldM(
        fail => env.console.putStrLn(s"failed $fail") *> ZIO.succeed(1),
        _ => env.console.putStrLn(s"succeeded") *> ZIO.succeed(0)
      )
    })
}
