package zio.config

import zio.config.ConfigDescriptor._
import zio.random.Random
import zio.test.Assertion._
import zio.test._
import zio.ZIO
import zio.{Has, ZManaged}
import java.util.concurrent.atomic.AtomicInteger
import zio.UIO
import MemoizedSourceSpecUtils._

object MemoizedSourceSpec extends BaseSpec {
  val spec: Spec[Has[TestConfig.Service] with Has[Random.Service], TestFailure[ReadError[String]], TestSuccess] =
    suite(
      "ConfigSource memoization and lazy config gets"
    )(
      // testM("A non-memoized source runs a resource acquisition for each config retrieval, and get config lazily") {
      //   val config = (string("k1") |@| string("k2")).tupled
      //   assertM(read(config from effectFulSource(releaseResource = false)))(equalTo((s"v1_1_1", "v2_2_2")))
      // }
      testM(
        "A non-memoized source runs a resource release before the next config retrieval while getting the configs lazily"
      ) {
        val config = (string("k1") |@| string("k2")).tupled
        assertM(read(config from effectFulSource(releaseResource = true)))(equalTo((s"v1_1_1", "v2_2_1")))
      }
    )
}

object MemoizedSourceSpecUtils {

  val Resource =
    new AtomicInteger(0)

  val configCount =
    new AtomicInteger(0)

  def tree(configCount: Int, resourceCount: Int, path: PropertyTreePath[String]): PropertyTree[String, String] =
    PropertyTree
      .Record(
        Map(
          "k1" -> PropertyTree.Leaf(s"v1_${configCount}_${resourceCount}"),
          "k2" -> PropertyTree.Leaf(s"v2_${configCount}_${resourceCount}")
        )
      )
      .at(path)

  val acquire =
    ZIO.effectTotal({
      println("hurry")
      Resource.incrementAndGet()
    })

  val release =
    ZIO.effectTotal({
      println("hurry2")
      Resource.decrementAndGet()
    })

  val logConfigValueFetch =
    ZIO.effectTotal(configCount.incrementAndGet())

  def effectFulSource(releaseResource: Boolean): ConfigSource = {
    val managed: ZManaged[Any, ReadError[K], PropertyTreePath[String] => UIO[PropertyTree[String, String]]] =
      ZManaged
        .make(acquire) { _ =>
          if (releaseResource) release else ZIO.unit
        }
        .map(resourceCount =>
          (path: PropertyTreePath[String]) =>
            logConfigValueFetch.map(configCount => tree(configCount, resourceCount, path))
        )

    ConfigSource.Reader(
      Set(ConfigSource.ConfigSourceName("effectful-source")),
      ZManaged.succeed(managed)
    )
  }
}
