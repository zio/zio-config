package zio.config.typesafe

import zio.{Config, ConfigProvider}
import zio.config._
import zio.Config
import zio.Config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

final case class EmployeeDetails(employees: List[Employee], accountId: Int)

final case class Employee(
  name: String,
  state: Option[Either[Int, String]],
  confidence: Either[Either[Double, Int], String]
)

object EmployeeDetails {

  val employee: Config[Employee] =
    (string("name") zip
      int("state").orElseEither(string("state")).optional zip
      double("confidence")
        .orElseEither(int("confidence")) // The value can be Double or Int for key confidence
        .orElseEither(                   // If not Double or Int, then it could be string, but this time the key can be confidence, confidences or confs!
          string("confidence")
            .orElse(string("confidences"))
            .orElse(string("confs"))
        )).to[Employee]

  val employeeDetails: zio.Config[EmployeeDetails] =
    ((listOf(employee).nested("employees")).zip(int("accountId"))).to[EmployeeDetails].nested("details")

}

object NullAndOptionalConfig extends ZIOSpecDefault {
  def spec: Spec[Any, Config.Error] = suite("TypesafeConfig Null and Optional")(
    test("A config case which keys maybe null or optional") {
      val hoconSource =
        ConfigProvider.fromHoconString(
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

      val result = read(EmployeeDetails.employeeDetails from hoconSource)

      val expectedResult =
        EmployeeDetails(
          List(
            Employee("chris", Some(Left(151)), Right("High")),
            Employee("susan", None, Right("f")),
            Employee("jon", Some(Right("CA")), Left(Left(1.278))),
            Employee("martha", None, Right("Medium")),
          ),
          1000
        )

      assertZIO(result)(equalTo(expectedResult))
    }
  )
}
