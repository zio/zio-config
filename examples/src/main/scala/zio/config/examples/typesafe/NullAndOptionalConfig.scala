package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor
import zio.config.magnolia.ConfigDescriptorProvider.description
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
  val employee: ConfigDescriptor[String, String, Employee] =
    (string("name") |@|
      int("state").orElseEither(string("state")).optional |@|
      double("confidence")
        .orElseEither(int("confidence")) // The value can be Double or Integer for key confidence
        .orElseEither(
          string("confidence") // Or it could be string, but this time the key can be confidence, confidences or confs!
            .orElse(string("confidences"))
            .orElse(string("confs"))
        ))(
      Employee.apply,
      Employee.unapply
    )

  val employeeDetails: ConfigDescriptor[String, String, EmployeeDetails] =
    (nested("employees")(list(employee)) |@| int("accountId"))(EmployeeDetails.apply, EmployeeDetails.unapply)
}

object NullAndOptionalConfig extends App {
  val hocconSourceList =
    hocon(
      Right(
        """
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
          confidences : Medium
        },
        {
          name       : susan
          confs      : Low
        } 
      ]

      accountId = 1000
       """
      )
    )

  val runtime = new DefaultRuntime {}

  lazy val u = description[EmployeeDetails]

  val result = runtime.unsafeRun(read(employeeDetails from hocconSourceList).either)

  assert(
    result ==
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
  )

}
