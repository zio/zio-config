package zio.config

import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test.{TestConfig, _}
import zio.{Random, Scope, UIO, ZIO}

import java.util.concurrent.atomic.AtomicInteger

import MemoizedSourceSpecUtils._

object MemoizedSourceSpec extends BaseSpec {
  val spec: Spec[TestConfig with Random, TestFailure[ReadError[String]], TestSuccess] =
    suite(
      "ConfigSource memoization and lazy config gets"
    )(
      test(
        "A non-memoized source runs a resource release for each config value during a read"
      ) {
        val config = (string("k1") zip string("k2"))

        val resource =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val source =
          effectFulSource(acquire(resource), UIO.succeed(resource.get), incrementCount(configCount))

        val effect =
          for {
            v <- read(config from source)
            r <- ZIO.succeed(resource.get)
            c <- ZIO.succeed(configCount.get)
          } yield (v, r, c)

        assertM(effect)(equalTo(((s"v1_1_1", "v2_2_2"), 2, 2)))

      },
      test(
        "A non-memoized source runs a resource release for each config value during a read"
      ) {
        val resource =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val config =
          (string("k1") zip string("k2"))

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
      test(
        "A memoized source runs a resource acquisition and release only once during the read of multiple of config values"
      ) {
        val resource1 =
          new AtomicInteger(0)

        val resource2 =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val config =
          (string("k1") zip string("k2"))

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
      },
      test(
        "A strictlyOnce source runs a resource acquisition and release only once, regardless of the number of reads invoked"
      ) {
        val resource1 =
          new AtomicInteger(0)

        val resource2 =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val config =
          (string("k1") zip string("k2"))

        val source =
          effectFulSource(acquire(resource1), acquire(resource2), incrementCount(configCount)).strictlyOnce

        val effect =
          for {
            src          <- source
            result1      <- read(config from src)
            result2      <- read(config from src)
            acquireCount <- ZIO.succeed(resource1.get)
            releaseCount <- ZIO.succeed(resource2.get)
            count        <- ZIO.succeed(configCount.get)
          } yield (result1, result2, acquireCount, releaseCount, count)

        assertM(effect)(equalTo((("v1_1_1", "v2_1_2"), ("v1_1_3", "v2_1_4"), 1, 1, 4)))
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

  def acquire(resource: AtomicInteger): UIO[Int] =
    ZIO.succeed({
      resource.incrementAndGet()
    })

  def release(resource: AtomicInteger): UIO[Int] =
    ZIO.succeed({
      resource.decrementAndGet()
    })

  def incrementCount(configCount: AtomicInteger): UIO[Int] =
    ZIO.succeed(configCount.incrementAndGet())

  def effectFulSource(
    acquire: UIO[Int],
    release: UIO[Int],
    incrementConfig: UIO[Int]
  ): ConfigSource = {
    val managed: ZIO[Scope, ReadError[String], PropertyTreePath[String] => UIO[PropertyTree[String, String]]] =
      ZIO
        .acquireRelease(acquire) { _ =>
          release
        }
        .map(resourceCount =>
          (path: PropertyTreePath[String]) => incrementConfig.map(configCount => tree(path, resourceCount, configCount))
        )

    ConfigSource.Reader(
      Set(ConfigSource.ConfigSourceName("effectful-source")),
      ZIO.succeed(managed)
    )
  }
}
