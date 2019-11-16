package zio.config.examples

import zio.DefaultRuntime
import zio.config._, ConfigDescriptor._
import zio.config.actions.ConfigDocs._

object ReadWriteReport extends App {

  case class Password(value: String)
  case class UserPwd(name: String, pwd: Option[Password], abc: Option[String], value: Option[XYZ])
  case class Token(value: String, clientid: String)
  case class XYZ(xyz: String, someInteger: Either[Int, String])

  type ProdConfig = Either[UserPwd, Token]

  // An example where user provides a description once and for all, and use it for read, write, report!
  val config =
    ((string("usr") ? "Example: some-user" |@|
      string("pwd").xmap(Password)(_.value).optional ? "sec" |@|
      string("jhi").optional ? "Ex: ghi" |@|
      (string("xyz") |@| int("abc").orElseEither(string("def")))(XYZ.apply, XYZ.unapply).optional ? "Ex: ha")(
      UserPwd.apply,
      UserPwd.unapply
    ) orElseEither
      (string("auth_token") |@| string("clientid"))(Token.apply, Token.unapply)) ? "Prod Config"

  val runtime = new DefaultRuntime {}

  val userNamePassword =
    Map(
      "usr" -> "v1",
      "pwd" -> "v2",
      "abc" -> "1",
      "xyz" -> "v3"
    )

  val source =
    ConfigSource.fromMap(userNamePassword)

  val result: ProdConfig =
    runtime.unsafeRun(read(config from source)) // Equivalent to Config.fromMap(userNamePassword, config)

  assert(
    result == Left(UserPwd("v1", Some(Password("v2")), None, Some(XYZ("v3", Left(1)))))
  )

  // want to write back the config ?
  assert(
    write(config, result) ==
      Right(
        PropertyTree.Record(
          Map(
            "usr" -> PropertyTree.Leaf[String, String]("v1"),
            "pwd" -> PropertyTree.Leaf[String, String]("v2"),
            "xyz" -> PropertyTree.Leaf[String, String]("v3"),
            "abc" -> PropertyTree.Leaf[String, String]("1")
          )
        )
      )
  )

  assert(
    generateDocs(config) ==
      Or(
        And(
          And(
            And(
              PathDetails(
                "usr",
                Descriptions(List("value of type string", "Example: some-user", "Prod Config"))
              ),
              PathDetails(
                "pwd",
                Descriptions(List("value of type string", "optional value", "sec", "Prod Config"))
              )
            ),
            PathDetails(
              "jhi",
              Descriptions(List("value of type string", "optional value", "Ex: ghi", "Prod Config"))
            )
          ),
          And(
            PathDetails(
              "xyz",
              Descriptions(List("value of type string", "optional value", "Ex: ha", "Prod Config"))
            ),
            Or(
              PathDetails(
                "abc",
                Descriptions(List("value of type int", "optional value", "Ex: ha", "Prod Config"))
              ),
              PathDetails(
                "def",
                Descriptions(List("value of type string", "optional value", "Ex: ha", "Prod Config"))
              )
            )
          )
        ),
        And(
          PathDetails("auth_token", Descriptions(List("value of type string", "Prod Config"))),
          PathDetails("clientid", Descriptions(List("value of type string", "Prod Config")))
        )
      )
  )

  assert(
    generateDocsWithValue(config, result) ==
      Right(
        Or(
          And(
            And(
              And(
                PathDetails(
                  "usr",
                  DescriptionsWithValue(
                    Some("v1"),
                    Descriptions(List("value of type string", "Example: some-user", "Prod Config"))
                  )
                ),
                PathDetails(
                  "pwd",
                  DescriptionsWithValue(
                    Some("v2"),
                    Descriptions(List("value of type string", "optional value", "sec", "Prod Config"))
                  )
                )
              ),
              PathDetails(
                "jhi",
                DescriptionsWithValue(
                  None,
                  Descriptions(List("value of type string", "optional value", "Ex: ghi", "Prod Config"))
                )
              )
            ),
            And(
              PathDetails(
                "xyz",
                DescriptionsWithValue(
                  Some("v3"),
                  Descriptions(List("value of type string", "optional value", "Ex: ha", "Prod Config"))
                )
              ),
              Or(
                PathDetails(
                  "abc",
                  DescriptionsWithValue(
                    Some("1"),
                    Descriptions(List("value of type int", "optional value", "Ex: ha", "Prod Config"))
                  )
                ),
                PathDetails(
                  "def",
                  DescriptionsWithValue(
                    None,
                    Descriptions(List("value of type string", "optional value", "Ex: ha", "Prod Config"))
                  )
                )
              )
            )
          ),
          And(
            PathDetails(
              "auth_token",
              DescriptionsWithValue(None, Descriptions(List("value of type string", "Prod Config")))
            ),
            PathDetails(
              "clientid",
              DescriptionsWithValue(None, Descriptions(List("value of type string", "Prod Config")))
            )
          )
        )
      )
  )
}
