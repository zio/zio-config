package zio.config

import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test._
import zio.{Scope, UIO, ZIO}

import java.util.concurrent.atomic.AtomicInteger

import MemoizedSourceSpecUtils._

object MemoizedSourceSpec extends BaseSpec {
  val spec =
    suite(
      "ConfigSource memoization and lazy config gets"
    )(
      test(
        "A non-memoized source runs a resource release for each config value during a read"
      ) {
        val config = (string("k1") zip string("k2") zip list("k3")(string))

        val resource =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        // The acquisition of resource results in an increment of `resource` atomic
        // The release of resource results is a no-operation
        val source =
          effectFulSource(acquire(resource), ZIO.succeedBlocking(resource.get), incrementCount(configCount))

        val effect =
          for {
            // config has 2 variables, hence resource atomic variable get incremented twice
            v <- read(config from source)
            r <- ZIO.succeed(resource.get)
            c <- ZIO.succeed(configCount.get)
          } yield (v, r, c)

        // To get third element a list, a resource is acquired (making it to 3),
        // and for retrieving each element in the list (of size 2), a resource is acquired (4 and 5)
        assertM(effect)(equalTo(((s"v1_1_1", "v2_2_2", List("v3_4_4", "v4_5_5")), 5, 5)))

      },
      test(
        "A non-memoized source runs a resource release for each config value during a read"
      ) {
        val resource =
          new AtomicInteger(0)

        val configCount =
          new AtomicInteger(0)

        val config =
          (string("k1") zip string("k2") zip list("k3")(string))

        // The acquisition of resource results in an increment of `resource` atomic
        // The release of resource results in a decrement of `resource` atomic
        val source =
          effectFulSource(acquire(resource), release(resource), incrementCount(configCount))

        val effect =
          for {
            // config has 2 variables, hence two pairs of acquisition and release should happen
            v <- read(config from source)
            r <- ZIO.succeed(resource.get)
            c <- ZIO.succeed(configCount.get)
          } yield (v, r, c)

        assertM(effect)(equalTo((("v1_1_1", "v2_1_2", List("v3_1_4", "v4_1_5")), 0, 5)))
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
          (string("k1") zip string("k2") zip list("k3")(string))

        // The acquisition of resource results in an increment of `resource1` atomic
        // The release of resource results in an increment of `resource2` atomic
        val source =
          effectFulSource(acquire(resource1), acquire(resource2), incrementCount(configCount)).memoize

        val effect =
          for {
            // Regardless of two config keys, `resource1` is incremented only once (during acquisition)
            // and `resource2` is incremented only once (during release)
            result       <- read(config from source)
            acquireCount <- ZIO.succeed(resource1.get)
            releaseCount <- ZIO.succeed(resource2.get)
            count        <- ZIO.succeed(configCount.get)
          } yield (result, acquireCount, releaseCount, count)

        assertM(effect)(equalTo(((s"v1_1_1", "v2_1_2", List("v3_1_4", "v4_1_5")), 1, 1, 5)))
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
          (string("k1") zip string("k2") zip list("k3")(string))

        // The acquisition of resource results in an increment of `resource1` atomic
        // The release of resource results in an increment of `resource2` atomic
        val source =
          effectFulSource(acquire(resource1), acquire(resource2), incrementCount(configCount)).strictlyOnce

        val effect =
          for {
            // Regardless of reading two config keys twice, `resource1` is incremented only once (during acquisition)
            // and `resource2` is incremented only once (during release)
            src          <- source
            result1      <- read(config from src)
            result2      <- read(config from src)
            acquireCount <- ZIO.succeed(resource1.get)
            releaseCount <- ZIO.succeed(resource2.get)
            count        <- ZIO.succeed(configCount.get)
          } yield (result1, result2, acquireCount, releaseCount, count)

        assertM(effect)(
          equalTo(
            (("v1_1_1", "v2_1_2", List("v3_1_4", "v4_1_5")), ("v1_1_6", "v2_1_7", List("v3_1_9", "v4_1_10")), 1, 1, 10)
          )
        )
      }
    )
}

object MemoizedSourceSpecUtils {
  def tree(path: PropertyTreePath[String], resourceCount: Int, configCount: Int): PropertyTree[String, String] =
    PropertyTree
      .Record(
        Map(
          "k1" -> PropertyTree.Leaf(s"v1_${resourceCount}_${configCount}"),
          "k2" -> PropertyTree.Leaf(s"v2_${resourceCount}_${configCount}"),
          "k3" -> PropertyTree.Sequence(
            List(
              PropertyTree.Leaf(s"v3_${resourceCount}_${configCount}"),
              PropertyTree.Leaf(s"v4_${resourceCount}_${configCount}")
            )
          )
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
