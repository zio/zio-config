package zio.config.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import zio.config.{BaseSpec, _}
import zio.test.Assertion._
import zio.test.{Sized, _}
import zio.{Config, ConfigProvider}

import RefinedUtils._

object RefinedSpec extends BaseSpec {
  override def spec: Spec[TestConfig with Sized, Config.Error] =
    suite("Refine package")(
      test("RefineType can successfully read valid refined values from a given path") {
        check(KeyValue.gen) { keyValue =>
          val cfg =
            refineType[NonEmptyString](keyValue.k.underlying)

          val result =
            read(cfg from ConfigProvider.fromMap(Map(keyValue.k.underlying -> keyValue.v.underlying)))

          assertZIO(result)(equalTo(keyValue.v.value))
        }
      },
      test("RefineType returns ReadError for invalid values in a given path") {
        check(Key.gen) { key =>
          val cfg = refineType[NonEmptyString](key.underlying)

          val result =
            read(cfg from ConfigProvider.fromMap(Map(key.underlying -> "")))

          assertZIO(result.either)(isLeft)
        }
      }
    )
}

object RefinedUtils {
  final case class Key(value: NonEmptyString) {
    def underlying: String = value.value
  }

  object Key {
    val gen: Gen[Sized, Key] =
      Gen
        .alphaNumericStringBounded(1, 10)
        .map(string => Refined.unsafeApply[String, NonEmpty](string))
        .map(Key.apply)
  }

  final case class Value(value: NonEmptyString) {
    def underlying: String = value.value
  }

  object Value {
    val gen: Gen[Sized, Value] =
      Gen
        .alphaNumericStringBounded(1, 10)
        .map(string => Refined.unsafeApply[String, NonEmpty](string))
        .map(Value.apply)
  }

  final case class KeyValue(k: Key, v: Value)

  object KeyValue {
    val gen: Gen[Sized, KeyValue] =
      for {
        key   <- Key.gen
        value <- Value.gen
      } yield KeyValue(key, value)
  }
}
