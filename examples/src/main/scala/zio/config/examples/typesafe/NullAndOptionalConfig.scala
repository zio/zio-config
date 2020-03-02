package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon
import EmployeeDetails._
import zio.config.ConfigDescriptor._

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
  val employee: ConfigDescriptor[String, String, Employee] =
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

  val employeeDetails: ConfigDescriptor[String, String, EmployeeDetails] =
    nested("details") {
      (nested("employees")(list(employee)) |@| int("accountId"))(
        EmployeeDetails.apply,
        EmployeeDetails.unapply
      )
    }
}

object NullAndOptionalConfig extends App {
  // Take a look at state values, that can either exist, or be given a null
  val hocconSourceList =
    hocon(
      Right(
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
            state      : null
            confidence : Medium
          },
          {
            name       : susan
            confs      : Low
          } 
         ]

        accountId = 1000

      }
       """
      )
    )

  val runtime = new DefaultRuntime {}

  val result1 = runtime.unsafeRun(read(employeeDetails from hocconSourceList).either)

  val expectedResult =
    Right(
      EmployeeDetails(
        List(
          Employee("jon", Some(Right("CA")), Left(Left(1.278))),
          Employee("chris", Some(Left(151)), Right("High")),
          Employee("martha", None, Right("Medium")),
          Employee("susan", None, Right("Low"))
        ),
        1000
      )
    )

  assert(result1 == expectedResult)
}
