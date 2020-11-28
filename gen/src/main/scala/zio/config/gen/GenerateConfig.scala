package zio.config.gen

import zio.{Chunk, ZIO}
import zio.config._
import typesafe._
import zio.config.magnolia.{descriptor, name}
import zio.random.Random
import zio.stream.ZStream
import zio.test.Sized
import zio.test.magnolia.DeriveGen

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
   * @param config
   * @return
   */
  def generateConfig[A: DeriveGen](
    config: ConfigDescriptor[A]
  ): ZStream[zio.random.Random with zio.test.Sized, String, PropertyTree[String, String]] =
    DeriveGen[A].sample.flatMap(r => ZStream.fromEffect(ZIO.fromEither(write(config, r.value))))

  def generateConfigJson[A: DeriveGen](
    config: ConfigDescriptor[A]
  ): ZStream[zio.random.Random with zio.test.Sized, String, String] =
    generateConfig(config).map(_.toJson)

  def generateConfigHoconString[A: DeriveGen](
    config: ConfigDescriptor[A]
  ): ZStream[zio.random.Random with zio.test.Sized, String, String] =
    generateConfig(config).map(_.toHoconString)

  def generateConfigMap[A: DeriveGen](
    config: ConfigDescriptor[A],
    keyDelimiter: String = "."
  ): ZStream[Random with zio.test.Sized, String, Map[String, ::[String]]] =
    generateConfig(config).map(_.flattenString(keyDelimiter))

  implicit class RunOps[E, A](s: ZStream[Random with zio.test.Sized, E, A]) {
    def get(size: Int): ZStream[Any, E, A] =
      s.provideLayer(Sized.live(size) ++ Random.live)
  }

  implicit class UnsafeRunOps[E, A](s: ZStream[Random with zio.test.Sized, E, A]) {
    def unsafeRunChunk(n: Int): Chunk[A] = {
      val runtime = zio.Runtime.default
      runtime.unsafeRun(s.get(n).runCollect)
    }
  }
}

object GenerateConfig extends GenerateConfig

object Test extends App {
  sealed trait Region

  @name("ap-southeast")
  case object ApSouthEast2 extends Region

  @name("usEast")
  case object UsEast extends Region

  final case class Database(port: Int, url: String)
  final case class MyConfig(region: Region, database: Database)

  val result =
    generateConfigJson(descriptor[MyConfig]).unsafeRunChunk(1).head

  println(result)
}
