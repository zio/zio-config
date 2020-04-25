package zio.config.typesafe

import zio.config.ConfigDescriptor._
import zio.config.read
import zio.config.typesafe.EmployeeDetails._
import zio.config.typesafe.TypesafeConfigSource.fromHoconString
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, suite, test, _ }

final case class EmployeeDetails(employees: List[Employee], accountId: Int)

final case class Employee(
  name: String,
  state: Option[Either[Int, String]],
  confidence: Either[Either[Double, Int], String]
)

object EmployeeDetails {

  val employee =
    (string("name") |@|
      int("state").orElseEither(string("state")).optional |@|
      double("confidence")
        .orElseEither(int("confidence")) // The value can be Double or Int for key confidence
        .orElseEither(                   // If not Double or Int, then it could be string, but this time the key can be confidence, confidences or confs!
          string("confidence")
            .orElse(string("confidences"))
            .orElse(string("confs"))
        ))(
      Employee.apply,
      Employee.unapply
    )

  val employeeDetails =
    nested("details") {
      (nested("employees")(list(employee)) |@| int("accountId"))(
        EmployeeDetails.apply,
        EmployeeDetails.unapply
      )
    }
}

object NullAndOptionalConfig extends DefaultRunnableSpec {
  val spec = suite("TypesafeConfig Null and Optional")(
    test("A config case which keys maybe null or optional") {
      val hocconSourceList =
        fromHoconString(
          """details {
            |  employees = [{
            |    name: jon
            |    state: CA
            |    confidence: 1.278
            |  },
            |    {
            |      name: chris
            |      state: 151
            |      confidence: High
            |    },
            |    {
            |      name: martha
            |      confidence: Medium
            |    },
            |    {
            |      name: susan
            |      confs: f
            |    }
            |  ]
            |
            |  accountId = 1000
            |}""".stripMargin
        )

      val result = hocconSourceList match {
        case Left(value)   => Left(value)
        case Right(source) => read(employeeDetails from source)
      }

      val expectedResult =
        Right(
          EmployeeDetails(
            List(
              Employee("jon", Some(Right("CA")), Left(Left(1.278))),
              Employee("chris", Some(Left(151)), Right("High")),
              Employee("martha", None, Right("Medium")),
              Employee("susan", None, Right("f"))
            ),
            1000
          )
        )

      assert(result)(equalTo(expectedResult))
    }
  )
}
