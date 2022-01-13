package zio.config

import zio.config.PropertyType._
import zio.config.PropertyTypeTestUtils._
import zio.test.{Gen, _}

import java.io.File
import java.net.{URI, URL}

object PropertyTypeTestJVM extends BaseSpec {

  val spec: ZSpec[Environment, Failure] =
    suite("PropertyTypeJVM")(
      test(s"valid URI string roundtrip") {
        check(Gen.string)(assertValidRoundtrip(UriType, new URI(_)))
      },
      test(s"valid FileType string roundtrip") {
        check(Gen.string)(assertValidRoundtrip(FileType, new File(_)))
      },
      propertyTypeRoundtripSuite(
        typeInfo = "URL",
        propType = UrlType,
        genValid = genValidUrlString,
        parse = new URL(_)
      ),
      test("valid JavaFilePathType string roundtrip") {
        check(Gen.string)(assertValidRoundtrip(JavaFilePathType, java.nio.file.Paths.get(_)))
      }
    )
}
