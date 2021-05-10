package zio.config.examples.typesafe

import zio.config._
import zio.config.typesafe.TypesafeConfigSource.fromHoconString

import EmployeeDetails._
import ConfigDescriptor._

final case class EmployeeDetails(employees: List[Employee], accountId: Int)

final case class Employee(
  name: String,
  state: Option[Either[Int, String]],
  confidence: Either[Either[Double, Int], String]
)

object EmployeeDetails {

  /**
   * An example, where manual configuration program is much more richer, with more details involved in it.
   * This is one such situation where corresponding automatic description won't work. The automatic description
   * will be just based on keys, and you can ofcourse manipulate keys later on but there is times when you really
   * need to describe your little configurations
   */
  val employee: ConfigDescriptor[Employee] =
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

  val employeeDetails: ConfigDescriptor[EmployeeDetails] =
    nested("details") {
      (nested("employees")(list(employee)) |@| int("accountId"))(
        EmployeeDetails.apply,
        EmployeeDetails.unapply
      )
    }
}

object NullAndOptionalConfig extends App {
  // Take a look at state values, that can either exist, or be given a null
  val hocconSourceList: Either[ReadError[String], ConfigSource] =
    fromHoconString(
      """
       details {
          employees = [{
            name       : jon
            state      : CA
            confidence : 1.278
          },
          {
            name       : chris
            state      : 151
            confidence : High
          },
          {
            name       : martha
            confidence : Medium
          },
          {
            name       : susan
            confs      : f
          }
         ]

        accountId = 1000

      }
       """
    )

  val expectedResult: Right[Nothing, EmployeeDetails] =
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

  val result: Either[ReadError[String], EmployeeDetails] = hocconSourceList match {
    case Left(value)   => Left(value)
    case Right(source) => read(employeeDetails from source)
  }

  assert(result == expectedResult)
}
