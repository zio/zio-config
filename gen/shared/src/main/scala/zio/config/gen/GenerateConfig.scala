package zio.config.gen

import zio.Schedule.recurs
import zio.config._
import zio.stream.ZStream
import zio.test.Sized
import zio.test.magnolia.DeriveGen
import zio.{Chunk, Random, Unsafe, ZIO}

import scala.collection.Map
import typesafe._

trait GenerateConfig {

  /**
   * Generate an almost valid PropertyTree
   * given a `ConfigDescriptor[A]` and `DeriveGen[A]`.
   *
   * Note that `DeriveGen[A]` is going to be available only
   * if all the fields in `A` (if A is a case class) has an instance of
   * DeriveGen.
   *
   * Most of the zio-config supported types has `DeriveGen` instances
   * are already provided
   *
   * Example:
   *  {{{
   *  sealed trait Region
   *
   *   @name("ap-southeast")
   *   case object ApSouthEast2 extends Region
   *
   *   @name("usEast")
   *   case object UsEast extends Region
   *
   *   final case class Database(port: Int, url: java.net.URL)
   *   final case class MyConfig(region: Region, database: Database)
   *
   *   val result =
   *     generateConfig(descriptor[MyConfig], 2).unsafeRunChunk
   *
   *   println(result)
   *
   *   // yield
   *
   *   Chunk(
   *     Record(
   *         Map(
   *           region -> Leaf(ap-southeast),
   *           database -> Record(Map(port -> Leaf(4220), url -> Leaf(https://def))))
   *         ),
   *       Record(
   *         Map(region -> Leaf(usEast2),
   *         database -> Record(Map(port -> Leaf(5572), url -> Leaf(https://abc)))))
   *       )
   *  }}}
   *
   * Note that this is based on ConfigDescriptor and not a direct view of a case class/sealed-trait.
   * This is why you get almost a valid Json. If your entire config is statically represented (meaning,
   * no custom transformation especially using transformOrFail or transform, with maximised usage of sealed traits instead of
   * values that are known only at runtime), then the correctness increases in the
   * emitted value. This is because zio-config would know the behaviour of your Config better, if most of its parts
   * are statically represented.
   *
   * That said, we are yet to support refined types, and might fail to generate values
   * because of absence of `DeriveGen`  instance for `Refined[A, B]`.
   * However, providing explicit DeriveGen instances in program is a solution.
   *
   * Also note that the uniqueness in emitted random values is not guaranteed. For this reason you can give
   * appropriate `size` value and emit as many `PropertyTree`
   * until all the required scenarios are covered.
   */
  def generateConfig[A: DeriveGen](
    config: ConfigDescriptor[A],
    size: Int = 0
  ): ZStream[Sized, String, PropertyTree[String, String]] =
    DeriveGen[A].sample
      .repeat(recurs(Math.max(0, size - 1)))
      .flatMap(r => ZStream.fromZIO(ZIO.fromEither(write(config, r.get.value))))

  /**
   * Generate an almost valid configuration in HOCON format
   * given a `ConfigDescriptor[A]` and `DeriveGen[A]`.
   *
   * Note that `DeriveGen[A]` is going to be available only
   * if all the fields in `A` (if A is a case class) has an instance of
   * DeriveGen.
   *
   * Most of the zio-config supported types has `DeriveGen` instances
   * are already provided
   *
   * Example:
   *  {{{
   *  sealed trait Region
   *
   *   @name("ap-southeast")
   *   case object ApSouthEast2 extends Region
   *
   *   @name("usEast")
   *   case object UsEast extends Region
   *
   *   final case class Database(port: Int, url: java.net.URL)
   *   final case class MyConfig(region: Region, database: Database)
   *
   *   val result =
   *     generateConfigHoconString(descriptor[MyConfig], 1).unsafeRunChunk
   *
   *   println(result)
   *
   * }}}
   *
   * // yield
   *
   * {{{
   * Chunk(
   *   database {
   *     port="9427"
   *     url="file://def"
   *   }
   *   region=usEast
   * ,
   *
   *   database {
   *     port="180"
   *     url="https://def"
   *   }
   *   region=ap-southeast
   * )
   * }}}
   *
   * Note that this is based on ConfigDescriptor and not a direct Json/HOCON view of a case class/sealed-trait.
   * This is why you get almost a valid Json. If your entire config is statically represented (meaning,
   * no custom transformation especially using transformOrFail or transform, with maximised usage of sealed traits instead of
   * values that are known only at runtime), then the correctness increases in the
   * emitted value. This is because zio-config would know the behaviour of your Config better, if most of its parts
   * are statically represented.
   *
   * That said, we are yet to support refined types, and might fail to generate values
   * because of absence of `DeriveGen`  instance for `Refined[A, B]`.
   * However, providing explicit DeriveGen instances in program is a solution.
   *
   * Also note that the uniqueness in emitted random values is not guaranteed. For this reason you can give
   * appropriate `size` value and emit as many `PropertyTree`
   * until all the required scenarios are covered.
   */
  def generateConfigHoconString[A: DeriveGen](
    config: ConfigDescriptor[A],
    size: Int = 0
  ): ZStream[Sized, String, String] =
    generateConfig(config, size).map(_.toHoconString)

  /**
   * Generate an almost valid configuration in Json format
   * given a `ConfigDescriptor[A]` and `DeriveGen[A]`.
   *
   * Note that `DeriveGen[A]` is going to be available only
   * if all the fields in `A` (if A is a case class) has an instance of
   * DeriveGen.
   *
   * Most of the zio-config supported types has `DeriveGen` instances
   * are already provided
   *
   * Example:
   *  {{{
   *  sealed trait Region
   *
   *   @name("ap-southeast")
   *   case object ApSouthEast2 extends Region
   *
   *   @name("usEast")
   *   case object UsEast extends Region
   *
   *   final case class Database(port: Int, url: java.net.URL)
   *   final case class MyConfig(region: Region, database: Database)
   *
   *   val result =
   *     generateConfigJson(descriptor[MyConfig], 1).unsafeRunChunk
   *
   *   println(result)
   *
   * }}}
   *
   * // yield
   *
   * {{{
   *   Chunk({
   *     "database" : {
   *         "port" : "7625",
   *         "url" : "http://abc"
   *     },
   *     "region" : "usEast"
   * }
   * ,{
   *     "database" : {
   *         "port" : "6904",
   *         "url" : "https://abc"
   *     },
   *     "region" : "ap-southeast"
   * }
   * )
   * }}}
   *
   * Note that this is based on the ConfigDescriptor and not a direct Json view of a case class/sealed-trait.
   * This is why you get almost a valid Json. If your entire config is statically represented (meaning,
   * no custom transformation especially using transformOrFail or transform, with maximised usage of sealed traits instead of
   * values that are known only at runtime), then the correctness increases in the
   * emitted value. This is because zio-config would know the behaviour of your Config better, if most of its parts
   * are statically represented.
   *
   * That said, we are yet to support refined types, and might fail to generate values
   * because of absence of `DeriveGen`  instance for `Refined[A, B]`.
   * However, providing explicit DeriveGen instances in program is a solution.
   *
   * Also note that the uniqueness in emitted random values is not guaranteed. For this reason you can give
   * appropriate `size` value and emit as many `PropertyTree`
   * until all the required scenarios are covered.
   */
  def generateConfigJson[A: DeriveGen](
    config: ConfigDescriptor[A],
    size: Int = 0
  ): ZStream[Sized, String, String] =
    generateConfig(config, size).map(_.toJson)

  /**
   * Generate an almost valid configuration in Map format
   * given a `ConfigDescriptor[A]` and `DeriveGen[A]`.
   *
   * Note that `DeriveGen[A]` is going to be available only
   * if all the fields in `A` (if A is a case class) has an instance of
   * DeriveGen.
   *
   * Most of the zio-config supported types has `DeriveGen` instances
   * are already provided
   *
   * Example:
   *  {{{
   *  sealed trait Region
   *
   *   @name("ap-southeast")
   *   case object ApSouthEast2 extends Region
   *
   *   @name("usEast")
   *   case object UsEast extends Region
   *
   *   final case class Database(port: Int, url: java.net.URL)
   *   final case class MyConfig(region: Region, database: Database)
   *
   *   val result =
   *     generateConfigMap(descriptor[MyConfig], 1).unsafeRunChunk
   *
   *   println(result)
   *
   * }}}
   *
   * // yields
   *
   * {{{
   *   Chunk(
   *     Map(
   *      region -> List(ap-southeast),
   *      database.port -> List(7627),
   *      database.url -> List(https://def)
   *    ),
   *    Map(
   *      region -> List(usEast),
   *      database.port -> List(4119), database.url -> List(file://abc)))
   *
   * }}}
   *
   * Note that this is based on ConfigDescriptor and not a direct Json/HOCON view of a case class/sealed-trait.
   * This is why you get almost a valid Json. If your entire config is statically represented (meaning,
   * no custom transformation especially using transformOrFail or transform, with maximised usage of sealed traits instead of
   * values that are known only at runtime), then the correctness increases in the
   * emitted value. This is because zio-config would know the behaviour of your Config better, if most of its parts
   * are statically represented.
   *
   * That said, we are yet to support refined types, and might fail to generate values
   * because of absence of `DeriveGen`  instance for `Refined[A, B]`.
   * However, providing explicit DeriveGen instances in program is a solution.
   *
   * Also note that the uniqueness in emitted random values is not guaranteed. For this reason you can give
   * appropriate `size` value and emit as many `PropertyTree`
   * until all the required scenarios are covered.
   */
  def generateConfigMap[A: DeriveGen](
    config: ConfigDescriptor[A],
    size: Int,
    keyDelimiter: String = "."
  ): ZStream[Sized, String, Map[String, ::[String]]] =
    generateConfig(config, size).map(_.flattenString(keyDelimiter))

  implicit class UnsafeRunOps[E, A](s: ZStream[Sized, E, A]) {
    def unsafeRunChunk: Chunk[A] = Unsafe.unsafeCompat { implicit u =>
      val runtime = zio.Runtime.default
      runtime.unsafe.run(s.provideLayer(Sized.live(1)).runCollect).getOrThrowFiberFailure()
    }
  }
}

object GenerateConfig extends GenerateConfig
