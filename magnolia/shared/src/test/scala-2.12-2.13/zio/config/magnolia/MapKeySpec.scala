package zio.config.magnolia

import zio.ConfigProvider
import zio.test.Assertion._
import zio.test._

import java.util.UUID

object MapKeySpec extends ZIOSpecDefault {

  final case class Inner(x: Int, y: String)

  final case class WithUuidMap(entries: Map[UUID, Inner])

  def spec =
    suite("Map key decoding")(
      test("Map[String, Int] — backwards compatible") {
        val map    = Map(
          "entries.a" -> "1",
          "entries.b" -> "2"
        )
        val config = deriveConfig[Map[String, Int]]
        assertZIO(ConfigProvider.fromMap(map).load(config.nested("entries")))(
          equalTo(Map("a" -> 1, "b" -> 2))
        )
      },
      test("Map[UUID, String] — UUID keys parsed from config") {
        check(Gen.uuid, Gen.uuid) { case (uuid1, uuid2) =>
          val map    = Map(
            s"entries.$uuid1" -> "alpha",
            s"entries.$uuid2" -> "beta"
          )
          val config = deriveConfig[Map[UUID, String]]
          assertZIO(ConfigProvider.fromMap(map).load(config.nested("entries")))(
            equalTo(Map(uuid1 -> "alpha", uuid2 -> "beta"))
          )
        }
      },
      test("Map[Int, String] — Int keys parsed from config") {
        val map    = Map(
          "entries.1" -> "one",
          "entries.2" -> "two"
        )
        val config = deriveConfig[Map[Int, String]]
        assertZIO(ConfigProvider.fromMap(map).load(config.nested("entries")))(
          equalTo(Map(1 -> "one", 2 -> "two"))
        )
      },
      test("Case class containing Map[UUID, CaseClass] — automatic derivation") {
        check(Gen.uuid, Gen.uuid) { case (uuid1, uuid2) =>
          val map    = Map(
            s"entries.$uuid1.x" -> "10",
            s"entries.$uuid1.y" -> "hello",
            s"entries.$uuid2.x" -> "20",
            s"entries.$uuid2.y" -> "world"
          )
          val config = deriveConfig[WithUuidMap]
          assertZIO(ConfigProvider.fromMap(map).load(config))(
            equalTo(
              WithUuidMap(
                Map(
                  uuid1 -> Inner(10, "hello"),
                  uuid2 -> Inner(20, "world")
                )
              )
            )
          )
        }
      },
      test("Invalid UUID key — error message propagation") {
        val map    = Map(
          "entries.not-a-uuid" -> "value"
        )
        val config = deriveConfig[Map[UUID, String]]
        assertZIO(ConfigProvider.fromMap(map).load(config.nested("entries")).either)(
          isLeft
        )
      }
    )
}
