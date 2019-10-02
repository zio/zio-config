package zio.config.examples

import zio.DefaultRuntime
import zio.config._

/**
 * An example that shows the usage of pretty much all existing combinators.
 * <+> , |@|, | etc*
 *
 * It also shows:
 *  1) How documentation is done.
 *  2) How config is Read
 *  3) How config is Written back
 *  4) How to only Report config
 */
object ReadWriteReport extends App {

  case class Password(value: String)
  case class UserPwd(name: String, pwd: Option[Password], anonymouse: Option[String])
  case class Token(value: String, clientid: String)

  type ProdConfig = Either[UserPwd, Token]

  import Config._

  // An example where user provides a description once and for all, and use it for read, write, report!
  val config: Config[ProdConfig] =
    (string("user") |@| string("pwd").xmap(Password)(_.value).optional |@| string("anonymous").optional)(
      UserPwd.apply,
      UserPwd.unapply
    ) or
      (string("auth_token") |@| string("clientid"))(Token.apply, Token.unapply)

  val runtime = new DefaultRuntime {}

  val userNamePassword =
    Map(
      "user" -> "v1",
      "pwd"  -> "v2"
    )

  val source =
    mapSource(userNamePassword)

  val result =
    runtime.unsafeRun(read(config).run.provide(source).map(_._2))

  assert(
    result == Left(UserPwd("v1", Some(Password("v2")), None))
  )

  val value = runtime.unsafeRun(report(config).run.provide(source))

  // Want docs ?
  assert(
    runtime.unsafeRun(report(config).run.provide(source)) ==
      ConfigReport(
        List(
          Details("pwd", "v2", "value of type string"),
          Details("user", "v1", "value of type string")
        )
      )
  )

  // want to write back the config ?
  assert(
    runtime.unsafeRun(write(config).run.provide(result)) ==
      Map(
        "user" -> "v1",
        "pwd"  -> "v2"
      )
  )
}
