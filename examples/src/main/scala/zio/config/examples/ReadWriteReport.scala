package zio.config.examples

import zio.DefaultRuntime
import zio.config._
import ConfigDescriptor._
import zio.config.ConfigDocs.Details._
import zio.config.ConfigDocs._
import ConfigSource._

object ReadWriteReport extends App {

  case class Password(value: String)
  case class UserPwd(name: String, pwd: Option[Password], abc: Option[String], value: Option[XYZ])
  case class Token(value: String, clientid: String)
  case class XYZ(xyz: String, someInteger: Either[Int, String])

  type ProdConfig = Either[UserPwd, Token]

  // An example where user provides a description once and for all, and use it for read, write, report!
  val configWithoutSource =
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

  val config = configWithoutSource from source

  val result: ProdConfig =
    runtime.unsafeRun(read(config)) // Equivalent to Config.fromMap(userNamePassword, config)

  assert(
    result == Left(UserPwd("v1", Some(Password("v2")), None, Some(XYZ("v3", Left(1)))))
  )

  // want to write back the config ?
  assert(
    write(config, result) ==
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

  assert(
    generateDocs(config) ==
      OneOf(
        Both(
          Both(
            Both(
              Path(
                "usr",
                Descriptions(
                  List(EmptySource, ConstantMap, "value of type string", "Example: some-user", "Prod Config")
                )
              ),
              Path(
                "pwd",
                Descriptions(
                  List(EmptySource, ConstantMap, "value of type string", "optional value", "sec", "Prod Config")
                )
              )
            ),
            Path(
              "jhi",
              Descriptions(
                List(EmptySource, ConstantMap, "value of type string", "optional value", "Ex: ghi", "Prod Config")
              )
            )
          ),
          Both(
            Path(
              "xyz",
              Descriptions(
                List(EmptySource, ConstantMap, "value of type string", "optional value", "Ex: ha", "Prod Config")
              )
            ),
            OneOf(
              Path(
                "abc",
                Descriptions(
                  List(EmptySource, ConstantMap, "value of type int", "optional value", "Ex: ha", "Prod Config")
                )
              ),
              Path(
                "def",
                Descriptions(
                  List(EmptySource, ConstantMap, "value of type string", "optional value", "Ex: ha", "Prod Config")
                )
              )
            )
          )
        ),
        Both(
          Path("auth_token", Descriptions(List(EmptySource, ConstantMap, "value of type string", "Prod Config"))),
          Path("clientid", Descriptions(List(EmptySource, ConstantMap, "value of type string", "Prod Config")))
        )
      )
  )

  assert(
    generateDocsWithValue(config, result) ==
      Right(
        OneOf(
          Both(
            Both(
              Both(
                Path(
                  "usr",
                  DescriptionsWithValue(
                    Some("v1"),
                    (List(EmptySource, ConstantMap, "value of type string", "Example: some-user", "Prod Config"))
                  )
                ),
                Path(
                  "pwd",
                  DescriptionsWithValue(
                    Some("v2"),
                    (List(EmptySource, ConstantMap, "value of type string", "optional value", "sec", "Prod Config"))
                  )
                )
              ),
              Path(
                "jhi",
                DescriptionsWithValue(
                  None,
                  (List(EmptySource, ConstantMap, "value of type string", "optional value", "Ex: ghi", "Prod Config"))
                )
              )
            ),
            Both(
              Path(
                "xyz",
                DescriptionsWithValue(
                  Some("v3"),
                  (List(EmptySource, ConstantMap, "value of type string", "optional value", "Ex: ha", "Prod Config"))
                )
              ),
              OneOf(
                Path(
                  "abc",
                  DescriptionsWithValue(
                    Some("1"),
                    (List(EmptySource, ConstantMap, "value of type int", "optional value", "Ex: ha", "Prod Config"))
                  )
                ),
                Path(
                  "def",
                  DescriptionsWithValue(
                    None,
                    (List(EmptySource, ConstantMap, "value of type string", "optional value", "Ex: ha", "Prod Config"))
                  )
                )
              )
            )
          ),
          Both(
            Path(
              "auth_token",
              DescriptionsWithValue(None, List(EmptySource, ConstantMap, "value of type string", "Prod Config"))
            ),
            Path(
              "clientid",
              DescriptionsWithValue(None, List(EmptySource, ConstantMap, "value of type string", "Prod Config"))
            )
          )
        )
      )
  )
}
