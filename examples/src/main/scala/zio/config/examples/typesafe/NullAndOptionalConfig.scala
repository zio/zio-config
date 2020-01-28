package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon
//import EmployeeDetails._
import zio.config.ConfigDescriptor._

final case class EmployeeDetails(employees: List[Employee], r: Int)
final case class Employee(name: String, state: Option[String], confidence: String)

object EmployeeDetails {

  val d: ConfigDescriptor[String, String, Employee] =
    (string("name") |@| string("state").optional |@| string("confidence").orElse(string("confidences")))(
      Employee.apply,
      Employee.unapply
    )

  val desc: ConfigDescriptor[String, String, EmployeeDetails] =
    (nested("employees")(list(d)) |@| int("r"))(EmployeeDetails.apply, EmployeeDetails.unapply)
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
          state      : NSW
          confidence : High
        },
        {
          name       : martha
          state      : CA
          confidences : Medium
        },
        {
          name       : susan
          state      : CA
          confidence : Low
        } 
      ]

      r = 10
       """
      )
    )

  val runtime = new DefaultRuntime {}

  lazy val u = description[EmployeeDetails]

  val result = runtime.unsafeRun(read(EmployeeDetails.desc from hocconSourceList).either)

  println(result)

  assert(
    result ==
      Right(
        EmployeeDetails(
          List(
            Employee("jon", Some("CA"), "1.278"),
            Employee("chris", Some("NSW"), "High"),
            Employee("martha", None, "Medium"),
            Employee("susan", None, "Low")
          ),
          10
        )
      )
  )

}
