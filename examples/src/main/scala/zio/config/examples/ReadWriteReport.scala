package zio.config.examples

import zio.DefaultRuntime
import zio.config._, Config._
import zio.config.actions.ConfigDocs
import zio.config.actions.ConfigDocs.KeyDescription

object ReadWriteReport extends App {

  case class Password(value: String)
  case class UserPwd(name: String, pwd: Option[Password], abc: Option[String], value: Option[XYZ])
  case class Token(value: String, clientid: String)
  case class XYZ(xyz: String, someInteger: Int)

  type ProdConfig = Either[UserPwd, Token]

  // An example where user provides a description once and for all, and use it for read, write, report!
  val config: ConfigDescriptor[ProdConfig] =
    ((string("usr") ~ "Example: some-user" |@|
      string("pwd").xmap(Password)(_.value).optional ~ "We don't care" ~ "yea !" |@|
      string("jhi").optional ~ "Example: ghi" |@|
      (string("xyz") |@| int("abc"))(XYZ.apply, XYZ.unapply).optional ~ "Example: xyz")(
      UserPwd.apply,
      UserPwd.unapply
    ) or
      (string("auth_token") |@| string("clientid"))(Token.apply, Token.unapply)) ~ "Prod Config"

  val runtime = new DefaultRuntime {}

  val userNamePassword =
    Map(
      "usr" -> "v1",
      "pwd" -> "v2"
    )

  val source =
    mapSource(userNamePassword)

  val result =
    runtime.unsafeRun(read(config).provide(source).map(_._2))

  assert(
    result == Left(UserPwd("v1", Some(Password("v2")), None, None))
  )

  val value = runtime.unsafeRun(read(config).provide(source).map(_._1))

  // Want report ?
  assert(
    value ==
      ConfigReport(
        List(
          Details("pwd", "v2", "value of type string"),
          Details("usr", "v1", "value of type string")
        )
      )
  )

  // want to write back the config ?
  assert(
    runtime.unsafeRun(write(config).run.provide(result)) ==
      Map(
        "usr" -> "v1",
        "pwd" -> "v2"
      )
  )

  // Want to get a man page for config
  assert(
    docs(config) ==
      ConfigDocs(
        List(
          KeyDescription("usr", List("value of type string", "Example: some-user", "Prod Config")),
          KeyDescription(
            "pwd",
            List("value of type string", "Optional value", "We don't care", "yea !", "Prod Config")
          ),
          KeyDescription("jhi", List("value of type string", "Optional value", "Example: ghi", "Prod Config")),
          KeyDescription("xyz", List("value of type string", "Optional value", "Example: xyz", "Prod Config")),
          KeyDescription("abc", List("value of type int", "Optional value", "Example: xyz", "Prod Config"))
        ),
        Some(
          ConfigDocs(
            List(
              KeyDescription("auth_token", List("value of type string", "Prod Config")),
              KeyDescription("clientid", List("value of type string", "Prod Config"))
            ),
            None
          )
        )
      )
  )
}
