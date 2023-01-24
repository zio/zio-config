package zio.config.examples

import zio.ZIO
import zio.config._

import ConfigDescriptor._

object ReadWriteReportExample extends App {

  case class Password(value: String)
  case class UserPwd(name: String, pwd: Option[Password], abc: Option[String], value: Option[XYZ])
  case class Token(value: String, clientid: String)
  case class XYZ(xyz: String, someInteger: Either[Int, String])

  type ProdConfig = Either[UserPwd, Token]

  // An example where user provides a description once and for all, and use it for read, write, report!
  val configWithoutSource: ConfigDescriptor[Either[UserPwd, Token]] =
    (string("usr") ?? "Example: some-user" zip
      string("pwd").to[Password].optional ?? "sec" zip
      string("jhi").optional ?? "Ex: ghi" zip
      (string("xyz") zip int("abc").orElseEither(string("def"))).to[XYZ].optional ?? "Ex: ha").to[UserPwd] orElseEither
      (string("auth_token") zip string("clientid")).to[Token] ?? "Prod Config"

  val runtime = zio.Runtime.default

  val userNamePassword: Map[String, String] =
    Map(
      "usr" -> "v1",
      "pwd" -> "v2",
      "abc" -> "1",
      "xyz" -> "v3"
    )

  val source: ConfigSource =
    ConfigSource.fromMap(userNamePassword)

  val config: ConfigDescriptor[Either[UserPwd, Token]] = configWithoutSource from source

  val result: ZIO[Any, ReadError[String], Either[UserPwd, Token]] =
    read(config) // Equivalent to Config.fromMap(userNamePassword, config)

  val expected: Left[UserPwd, Nothing] =
    Left(UserPwd("v1", Some(Password("v2")), None, Some(XYZ("v3", Left(1)))))

  assert(
    result equalM expected
  )

  assert(
    write(config, expected)
      .map(_.flattenKeyAndValue()) ==
      Right(
        Map("usr" -> "v1", "pwd" -> "v2", "xyz" -> "v3", "abc" -> "1")
      )
  )

  assert(
    write(config, expected) ==
      Right(
        PropertyTree.Record(
          Map(
            "usr" -> PropertyTree.Leaf("v1"),
            "pwd" -> PropertyTree.Leaf("v2"),
            "xyz" -> PropertyTree.Leaf("v3"),
            "abc" -> PropertyTree.Leaf("1")
          )
        )
      )
  )

}
