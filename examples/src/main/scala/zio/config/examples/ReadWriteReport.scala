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
    ((string("usr") ?? "Example: some-user" |@|
      string("pwd").xmap(Password)(_.value).optional ?? "sec" |@|
      string("jhi").optional ?? "Ex: ghi" |@|
      (string("xyz") |@| int("abc").orElseEither(string("def")))(XYZ.apply, XYZ.unapply).optional ?? "Ex: ha")(
      UserPwd.apply,
      UserPwd.unapply
    ) orElseEither
      (string("auth_token") |@| string("clientid"))(Token.apply, Token.unapply)) ?? "Prod Config"

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

  val result =
    read(config) // Equivalent to Config.fromMap(userNamePassword, config)

  val expected =
    Left(UserPwd("v1", Some(Password("v2")), None, Some(XYZ("v3", Left(1)))))

  assert(
    result == Right(expected)
  )

  assert(
    write(config, expected)
      .map(_.flattenKeyAndValue()) ==
      Right(
        Map("usr" -> "v1", "pwd" -> "v2", "xyz" -> "v3", "abc" -> "1")
      )
  )

  import PropertyTree._

  assert(
    write(config, expected) ==
      Right(
        Sequence(
          List(
            Sequence(
              List(
                Sequence(List(Record(Map("usr" -> Leaf("v1"), "pwd" -> Leaf("v2"))))),
                Record(Map("xyz" -> Leaf("v3"), "abc" -> Leaf("1")))
              )
            )
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
                  Sources(List(EmptySource, ConstantMap)),
                  List("value of type string", "Example: some-user", "Prod Config")
                )
              ),
              Path(
                "pwd",
                Descriptions(
                  Sources(List(EmptySource, ConstantMap)),
                  List("value of type string", "optional value", "sec", "Prod Config")
                )
              )
            ),
            Path(
              "jhi",
              Descriptions(
                Sources(List(EmptySource, ConstantMap)),
                List("value of type string", "optional value", "Ex: ghi", "Prod Config")
              )
            )
          ),
          Both(
            Path(
              "xyz",
              Descriptions(
                Sources(List(EmptySource, ConstantMap)),
                List("value of type string", "optional value", "Ex: ha", "Prod Config")
              )
            ),
            OneOf(
              Path(
                "abc",
                Descriptions(
                  Sources(List(EmptySource, ConstantMap)),
                  List("value of type int", "optional value", "Ex: ha", "Prod Config")
                )
              ),
              Path(
                "def",
                Descriptions(
                  Sources(List(EmptySource, ConstantMap)),
                  List("value of type string", "optional value", "Ex: ha", "Prod Config")
                )
              )
            )
          )
        ),
        Both(
          Path(
            "auth_token",
            Descriptions(Sources(List(EmptySource, ConstantMap)), List("value of type string", "Prod Config"))
          ),
          Path(
            "clientid",
            Descriptions(Sources(List(EmptySource, ConstantMap)), List("value of type string", "Prod Config"))
          )
        )
      )
  )

  assert(
    generateDocsWithValue(config, expected) ==
      Right(
        OneOf(
          Both(
            Both(
              Both(
                Path(
                  "usr",
                  DescriptionsWithValue(
                    Some("v1"),
                    Sources(List(EmptySource, ConstantMap)),
                    List("value of type string", "Example: some-user", "Prod Config")
                  )
                ),
                Path(
                  "pwd",
                  DescriptionsWithValue(
                    Some("v2"),
                    Sources(List(EmptySource, ConstantMap)),
                    List("value of type string", "optional value", "sec", "Prod Config")
                  )
                )
              ),
              Path(
                "jhi",
                DescriptionsWithValue(
                  None,
                  Sources(List(EmptySource, ConstantMap)),
                  List("value of type string", "optional value", "Ex: ghi", "Prod Config")
                )
              )
            ),
            Both(
              Path(
                "xyz",
                DescriptionsWithValue(
                  Some("v3"),
                  Sources(List(EmptySource, ConstantMap)),
                  List("value of type string", "optional value", "Ex: ha", "Prod Config")
                )
              ),
              OneOf(
                Path(
                  "abc",
                  DescriptionsWithValue(
                    Some("1"),
                    Sources(List(EmptySource, ConstantMap)),
                    List("value of type int", "optional value", "Ex: ha", "Prod Config")
                  )
                ),
                Path(
                  "def",
                  DescriptionsWithValue(
                    None,
                    Sources(List(EmptySource, ConstantMap)),
                    List("value of type string", "optional value", "Ex: ha", "Prod Config")
                  )
                )
              )
            )
          ),
          Both(
            Path(
              "auth_token",
              DescriptionsWithValue(
                None,
                Sources(List(EmptySource, ConstantMap)),
                List("value of type string", "Prod Config")
              )
            ),
            Path(
              "clientid",
              DescriptionsWithValue(
                None,
                Sources(List(EmptySource, ConstantMap)),
                List("value of type string", "Prod Config")
              )
            )
          )
        )
      )
  )
}
