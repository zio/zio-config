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
      testM(
        "A non-memoized source runs a resource acquisition for each config retrieval"
      ) {
        val config = (string("k1") |@| string("k2")).tupled

        val resource =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val source =
          effectFulSource(acquire(resource), UIO(resource.get), incrementCount(configCount))

        val effect =
          for {
            v <- read(config from source)
            r <- ZIO.succeed(resource.get)
            c <- ZIO.succeed(configCount.get)
          } yield (v, r, c)

        assertM(effect)(equalTo(((s"v1_1_1", "v2_2_2"), 2, 2)))

      },
      testM(
        "A non-memoized source runs a resource release before the next config retrieval"
      ) {
        val resource =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val config =
          (string("k1") |@| string("k2")).tupled

        val source =
          effectFulSource(acquire(resource), release(resource), incrementCount(configCount))

        val effect =
          for {
            v <- read(config from source)
            r <- ZIO.succeed(resource.get)
            c <- ZIO.succeed(configCount.get)
          } yield (v, r, c)

        assertM(effect)(equalTo(((s"v1_1_1", "v2_1_2"), 0, 2)))
      },
      testM(
        "A memoized source runs a resource acquisition and release only once, while getting multiple configs incrementally"
      ) {
        val resource1 =
          new AtomicInteger(0)

        val resource2 =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val config =
          (string("k1") |@| string("k2")).tupled

        val source =
          effectFulSource(acquire(resource1), acquire(resource2), incrementCount(configCount)).memoize

        val effect =
          for {
            result       <- read(config from source)
            acquireCount <- ZIO.succeed(resource1.get)
            releaseCount <- ZIO.succeed(resource2.get)
            count        <- ZIO.succeed(configCount.get)
          } yield (result, acquireCount, releaseCount, count)

        assertM(effect)(equalTo(((s"v1_1_1", "v2_1_2"), 1, 1, 2)))
      }
    )
}

object MemoizedSourceSpecUtils {
  def tree(path: PropertyTreePath[String], resourceCount: Int, configCount: Int): PropertyTree[String, String] =
    PropertyTree
      .Record(
        Map(
          "k1" -> PropertyTree.Leaf(s"v1_${resourceCount}_${configCount}"),
          "k2" -> PropertyTree.Leaf(s"v2_${resourceCount}_${configCount}")
        )
      )
      .at(path)

  def acquire(resource: AtomicInteger) =
    ZIO.effectTotal({
      resource.incrementAndGet()
    })

  def release(resource: AtomicInteger) =
    ZIO.effectTotal({
      resource.decrementAndGet()
    })

  def incrementCount(configCount: AtomicInteger) =
    ZIO.effectTotal(configCount.incrementAndGet())

  def effectFulSource(
    acquire: UIO[Int],
    release: UIO[Int],
    incrementConfig: UIO[Int]
  ): ConfigSource = {
    val managed: ZManaged[Any, ReadError[K], PropertyTreePath[String] => UIO[PropertyTree[String, String]]] =
      ZManaged
        .make(acquire) { _ =>
          release
        }
        .map(resourceCount =>
          (path: PropertyTreePath[String]) => incrementConfig.map(configCount => tree(path, resourceCount, configCount))
        )

    ConfigSource.Reader(
      Set(ConfigSource.ConfigSourceName("effectful-source")),
      ZManaged.succeed(managed)
    )
  }
}
