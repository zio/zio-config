package zio.config

import zio.config.PropertyType._
import zio.config.PropertyTypeTestUtils._
import zio.test._

import java.io.File
import java.net.{URI, URL}

object PropertyTypeTestJVM extends BaseSpec {

  val spec: ZSpec[Environment, Failure] =
    suite("PropertyTypeJVM")(
      testM(s"valid URI string roundtrip") {
        check(Gen.anyString)(assertValidRoundtrip(UriType, new URI(_)))
      },
      testM(s"valid FileType string roundtrip") {
        check(Gen.anyString)(assertValidRoundtrip(FileType, new File(_)))
      },
      propertyTypeRoundtripSuite(
        typeInfo = "URL",
        propType = UrlType,
        genValid = genValidUrlString,
        parse = new URL(_)
      ),
      testM("valid JavaFilePathType string roundtrip") {
        check(Gen.anyString)(assertValidRoundtrip(JavaFilePathType, java.nio.file.Paths.get(_)))
      }
    )
}
