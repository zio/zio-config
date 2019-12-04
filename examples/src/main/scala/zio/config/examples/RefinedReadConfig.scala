package zio.config.examples

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import zio.config.ConfigDescriptor._
import zio.config._
import zio.config.refined._
import zio.{ App, ZEnv, ZIO }

case class RefinedProd(
  ldap: Refined[String, NonEmpty],
  port: Refined[Int, GreaterEqual[W.`1024`.T]],
  dburl: Option[Refined[String, NonEmpty]]
)

object RefinedProd {

  val prodConfig: ConfigDescriptor[String, String, RefinedProd] =
    (
      nonEmpty(string("LDAP")) |@|
        greaterEqual[W.`1024`.T](int("PORT")) |@|
        nonEmpty(string("DB_URL")).optional
    )(
      RefinedProd.apply,
      RefinedProd.unapply
    )

  val myAppLogic: ZIO[Config[RefinedProd], Throwable, (Refined[String, NonEmpty], Option[Refined[String, NonEmpty]])] =
    for {
      prod <- config[RefinedProd]
    } yield (prod.ldap, prod.dburl)
}

object RefinedReadConfig extends App {
  println(RefinedProd.prodConfig)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ZIO.accessM { env =>
      Config
        .fromMap(Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"), RefinedProd.prodConfig)
        .flatMap(config => RefinedProd.myAppLogic.provide(config))
        .foldM(failure => env.console.putStrLn(failure.toString) *> ZIO.succeed(1), _ => ZIO.succeed(0))
    }
}

object Codegen {
  final case class Wrapper(method: String, pred: String, desc: String)
  val helpers =
    List(
      Wrapper(
        "less",
        "Less",
        "/** Predicate that checks if a numeric value is less than `N` */"
      ),
      Wrapper(
        "greater",
        "Greater",
        "/** Predicate that checks if a numeric value is greater than `N` */"
      ),
      Wrapper(
        "lessEqual",
        "LessEqual",
        "/** Predicate that checks if a numeric value is less than or equal to `N` */"
      ),
      Wrapper(
        "greaterEqual",
        "GreaterEqual",
        "/** Predicate that checks if a numeric value is greater than or equal to `N` */"
      ),
      Wrapper(
        "divisible",
        "Divisible",
        "/** Predicate that checks if an integral value is evenly divisible by `N` */"
      ),
      Wrapper(
        "nonDivisible",
        "NonDivisible",
        "/** Predicate that checks if an integral value is not evenly divisible by `N` */"
      )
    )

  def partialWrapper: Wrapper => List[String] =
    w => {
      List(
        s"""final class ${w.pred}PartiallyApplied[N] {
           |  def apply[K, V, A](
           |    desc: ConfigDescriptor[K, V, A]
           |  )(
           |    implicit ev: Validate[A, ${w.pred}[N]]
           |  ): ConfigDescriptor[K, V, Refined[A, ${w.pred}[N]]] =
           |    asRefined[K, V, A, ${w.pred}[N]](desc)
           |}
         """.stripMargin
      )
    }
  def wrapperCall: Wrapper => List[String] =
    w =>
      List(
        s"  ${w.desc}",
        s"""  def ${w.method}[N]: ${w.pred}PartiallyApplied[N] =
           |    new ${w.pred}PartiallyApplied[N]
           """.stripMargin
      )

  def main(args: Array[String]): Unit = {
    val lines: List[String] =
      helpers.flatMap(partialWrapper) ++
        helpers.flatMap(wrapperCall)
    lines.foreach(println)
  }
}
