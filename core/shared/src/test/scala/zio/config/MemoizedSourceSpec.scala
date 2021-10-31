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
      testM("A non-memoized source runs a resource acquisition for each config retrieval, and get config lazily") {
        val config = (string("k1") |@| string("k2")).tupled

        val effect =
          for {
            _ <- resetEnv
            _  = println(resource.get())
            v <- read(config from effectFulSource(releaseResource = false))
            r <- ZIO.succeed(resource.get)
            c <- ZIO.succeed(configCount.get)
          } yield (v, r, c)

        assertM(effect)(equalTo(((s"v1", "v2"), 2, 2)))

      },
      testM(
        "A non-memoized source runs a resource release before the next config retrieval while getting the configs lazily"
      ) {
        val config = (string("k1") |@| string("k2")).tupled

        val effect =
          for {
            _ <- resetEnv
            _  = println("hello " + resource.get())
            v <- read(config from effectFulSource(releaseResource = true))
            r <- ZIO.succeed(resource.get)
            c <- ZIO.succeed(configCount.get)
          } yield (v, r, c)

        assertM(effect)(equalTo(((s"v1", "v2"), 0, 2)))

      }
    )
}

object MemoizedSourceSpecUtils {
  val resource =
    new AtomicInteger(0)

  val configCount =
    new AtomicInteger(0)

  def resetEnv = ZIO.effectTotal({
    resource.set(0)
    configCount.set(0)
  })

  def tree(path: PropertyTreePath[String]): PropertyTree[String, String] =
    PropertyTree
      .Record(
        Map(
          "k1" -> PropertyTree.Leaf(s"v1"),
          "k2" -> PropertyTree.Leaf(s"v2")
        )
      )
      .at(path)

  val acquire =
    ZIO.effectTotal({
      resource.incrementAndGet()
    })

  val release =
    ZIO.effectTotal({
      resource.decrementAndGet()
    })

  val logConfigValueFetch =
    ZIO.effectTotal(configCount.incrementAndGet())

  def effectFulSource(releaseResource: Boolean): ConfigSource = {
    val managed: ZManaged[Any, ReadError[K], PropertyTreePath[String] => UIO[PropertyTree[String, String]]] =
      ZManaged
        .make(acquire) { _ =>
          if (releaseResource) release else ZIO.unit
        }
        .map(_ => (path: PropertyTreePath[String]) => logConfigValueFetch.map(_ => tree(path)))

    ConfigSource.Reader(
      Set(ConfigSource.ConfigSourceName("effectful-source")),
      ZManaged.succeed(managed)
    )
  }
}
