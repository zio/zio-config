package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.Config._
import zio.config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}
import zio.{Config, ConfigProvider}

import java.util.Properties

final case class EmployeeDetails(employees: List[Employee], accountId: Int, boolean: Boolean, optional: Option[String])

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
    ((listOf(employee)
      .nested("employees"))
      .zip(int("accountId"))
      .zip(Config.boolean("boolean"))
      .zip(Config.string("optional").optional))
      .to[EmployeeDetails]
      .nested("details")

}

object TypesafeConfigSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Config.Error] = suite("TypesafeConfigSpec")(
    test("Retrieves default values inside an empty map") {
      val hoconSource =
        s"""
           | {
           |   keys : {
           |     key : [{}]
           |   }
           | }
           |
           |""".stripMargin

      val intBoolean: zio.Config[(Int, Boolean)] =
        int("foo").withDefault(1).zip(boolean("bar").withDefault(false))

      val mapConfig = Config.table("keys", Config.listOf(intBoolean))

      val io =
        ConfigProvider.fromHoconString(hoconSource).load(mapConfig)

      val expected =
        Map("key" -> List((1, false)))

      assertZIO(io)(equalTo(expected))

    },
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
            |  boolean = true
            |  accountId = 1000
            |  optional = null
            |}""".stripMargin
        )

      val result =
        hoconSource.load(EmployeeDetails.employeeDetails)

      val expectedResult =
        EmployeeDetails(
          List(
            Employee("chris", Some(Left(151)), Right("High")),
            Employee("jon", Some(Right("CA")), Left(Left(1.278))),
            Employee("susan", None, Right("f")),
            Employee("martha", None, Right("Medium"))
          ),
          1000,
          true,
          None
        )

      assertZIO(result.map(_.employees))(hasSameElements(expectedResult.employees))
    },
    test("Loads list of objects and primitives from numeric-indexed properties (issue #1575)") {
      final case class RedisNodeConfig(host: String, port: Int)
      final case class RedisClusterConfig(nodes: List[RedisNodeConfig], tags: List[String])

      val nodeConfig: Config[RedisNodeConfig] =
        (string("host") zip int("port")).to[RedisNodeConfig]

      val clusterConfig: Config[RedisClusterConfig] =
        (listOf("nodes", nodeConfig) zip listOf("tags", string)).to[RedisClusterConfig].nested("redis-cluster")

      val props = new Properties()
      props.setProperty("redis-cluster.nodes.0.host", "some-server")
      props.setProperty("redis-cluster.nodes.0.port", "6379")
      props.setProperty("redis-cluster.nodes.1.host", "another-server")
      props.setProperty("redis-cluster.nodes.1.port", "6380")
      props.setProperty("redis-cluster.tags.0", "primary")
      props.setProperty("redis-cluster.tags.1", "replica")

      val provider = TypesafeConfigProvider.fromTypesafeConfig(
        ConfigFactory.parseProperties(props)
      )

      val result = provider.load(clusterConfig)

      val expected = RedisClusterConfig(
        List(
          RedisNodeConfig("some-server", 6379),
          RedisNodeConfig("another-server", 6380)
        ),
        List("primary", "replica")
      )

      assertZIO(result)(equalTo(expected))
    }
  )
}
