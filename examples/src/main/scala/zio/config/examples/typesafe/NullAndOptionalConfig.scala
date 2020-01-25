package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon
import EmployeeDetails._

final case class EmployeeDetails(employees: List[Employee])

object EmployeeDetails {
  final case class Employee(name: String, state: Option[String], confidence: String)

  val desc = description[EmployeeDetails]
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
          confidence : Medium
        },
        {
          name       : susan
          state      : null
          confidence : Low
        } 
      ]
       """
      )
    )

  val runtime = new DefaultRuntime {}

  val result = runtime.unsafeRun(read(desc from hocconSourceList).either)

  assert(
    result ==
      Right(
        EmployeeDetails(
          List(
            Employee("jon", Some("CA"), "1.278"),
            Employee("chris", Some("NSW"), "High"),
            Employee("martha", None, "Medium"),
            Employee("susan", None, "Low")
          )
        )
      )
  )

}
