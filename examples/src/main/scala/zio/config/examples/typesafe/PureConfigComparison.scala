package zio.config.examples.typesafe

import zio.{ DefaultRuntime, IO }
import zio.config.typesafe.TypeSafeConfigSource._
//import pureconfig.{ ConfigSource => PConfigSource }
import zio.config._
import zio.config.magnolia.ConfigDescriptorProvider.description

object PureConfigComparison extends App {
  val runtime = new DefaultRuntime {}
  def run[K, A](c: IO[ReadErrorsVector[K], A]): A =
    runtime.unsafeRun(c)

  val configString =
    """
      |exportDetails = [
      |  {
      |    table          : some_name
      |    columns        : [ a, b, c, d ]
      |    extraDetails = [
      |      {
      |        hi : di
      |        bi : ci
      |        r = [
      |          {
      |            ci : ki
      |            vi : bi
      |            lst: [1, 1, 1]
      |          }
      |          {
      |            ci : ki
      |            vi : 1.0
      |            lst: [1, 2, 1]
      |          }
      |           {
      |            ci : ki
      |            vi : 3
      |            lst: [1, 3, 5]
      |          }
      |
      |        ]
      |      }
      |    ]
      |  }
      |]
      |""".stripMargin

  final case class MoreDetail(ci: String, vi: Either[Double, String], lst: List[Int])
  final case class DbDetails(hi: String, bi: String, r: List[MoreDetail])
  final case class TableColumns(table: String, columns: List[String], extraDetails: List[DbDetails])
  final case class ExportDetails(exportDetails: List[TableColumns])

  /*************************************
   *
   *  With zio-config
   *
   *************************************
   */
  val zioConfigResult =
    run(read(description[ExportDetails] from hocon(Right(configString))))
  // result: ExportDetails(List(TableColumns(some_name,List(a, b, c, d),List(DbDetails(di,ci,List(MoreDetail(ki,Right(bi),List(1, 1, 1)), MoreDetail(ki,Left(1.0),List(1, 2, 1)), MoreDetail(ki,Left(3.0),List(1, 3, 5))))))))

  /*************************************
   *
   *  With pure-config (failed, version: 0.12.3)
   *
   *  pure-config assumbes kebab case by default, hence case class keys are invalid.
   *
   *  Also:
   *  Please note, currently the below code is commented out because of stack overflow exception.
   *  To try out, please uncomment, increase the memory requirements, else we get:
   *
   * sbt:root> compile
   * [error] java.lang.StackOverflowError
   * [error] scala.tools.nsc.typechecker.Macros.standardMacroExpand$(Macros.scala:783)
   * [error] scala.tools.nsc.Global$$anon$4.standardMacroExpand(Global.scala:482)
   *  ..........
   *  .........
   * [error] shapeless.LazyMacros$DerivationContext.$anonfun$derive$2(lazy.scala:507)
   * [error] scala.util.Either$LeftProjection.flatMap(Either.scala:561)
   * [error] shapeless.LazyMacros$DerivationContext.$anonfun$derive$1(lazy.scala:505)
   * [error] scala.Option.getOrElse(Option.scala:189)
   * [error] shapeless.LazyMacros$DerivationContext.derive(lazy.scala:505)
   * [error] shapeless.LazyMacros$DerivationContext$State$.deriveInstance(lazy.scala:337)
   * [error] shapeless.LazyMacrosCompat.deriveInstance(lazymacros.scala:50)
   * [error] shapeless.LazyMacrosCompat.deriveInstance$(lazymacros.scala:33)
   *************************************
   */
  // import pureconfig.generic.auto._

  // def pureConfigResult = PConfigSource.string(configString).load[ExportDetails]
  // res: Left(ConfigReaderFailures(ConvertFailure(KeyNotFound(export-details,Set(exportDetails)),None,),List()))
  // With pure-config : by default that means export-details in hoccon should be exportDetails in case class

  val fixedString =
  """
    |export-details = [
    |  {
    |    table          : some_name
    |    columns        : [ a, b, c, d ]
    |    extra-details = [
    |      {
    |        hi : di
    |        bi : ci
    |        r = [
    |          {
    |            ci : ki
    |            vi : bi
    |            lst: [1, 1, 1]
    |          }
    |          {
    |            ci : ki
    |            vi : 1.0
    |            lst: [1, 2, 1]
    |          }
    |           {
    |            ci : ki
    |            vi : 3
    |            lst: [1, 3, 5]
    |          }
    |
    |        ]
    |      }
    |    ]
    |  }
    |]
    |""".stripMargin

  /*************************************
   *
   *  With pure-config (failed, 0.12.3)
   *  Failure, it can't translate v1: Either[Double, String], as try to parse double, else parse it as string
   *  which is part of extra-details.r
   *
   *************************************
   */
  // def pureConfigResultAfterFixingKey = ConfigSource.string(fixedString).load[ExportDetails]
  // Left(ConfigReaderFailures(ConvertFailure(WrongType(STRING,Set(OBJECT)),None,export-details.0.extra-details.0.r.0.vi),List(ConvertFailure(WrongType(NUMBER,Set(OBJECT)),None,export-details.0.extra-details.0.r.1.vi), ConvertFailure(WrongType(STRING,Set(OBJECT)),None,export-details.0.extra-details.0.r.2.vi))))

  /*************************************
   *
   *  With zio-config
   *
   *************************************
   */
  // with zio-config-magnolia, and explicitly performing the key conversion
  val zioConfigWithKeysInKebab =
    run(
      read(description[ExportDetails].convertKey(KeyConversion.camelToKebab) from hocon(Right(fixedString)))
    )

  //result: ExportDetails(List(TableColumns(some_name,List(a, b, c, d),List(DbDetails(di,ci,List(MoreDetail(ki,Right(bi)), MoreDetail(ki,Left(1.0)), MoreDetail(ki,Left(3.0))))))))
}
