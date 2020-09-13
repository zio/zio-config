package zio.config.examples

import zio.config._, ConfigDescriptor._

object ReadWriteReportExample extends App {

  case class Password(value: String)
  case class UserPwd(name: String, pwd: Option[Password], abc: Option[String], value: Option[XYZ])
  case class Token(value: String, clientid: String)
  case class XYZ(xyz: String, someInteger: Either[Int, String])

  type ProdConfig = Either[UserPwd, Token]

  // An example where user provides a description once and for all, and use it for read, write, report!
  val configWithoutSource =
    ((string("usr") ?? "Example: some-user" |@|
      string("pwd")(Password.apply, Password.unapply).optional ?? "sec" |@|
      string("jhi").optional ?? "Ex: ghi" |@|
      (string("xyz") |@| int("abc").orElseEither(string("def")))(XYZ.apply, XYZ.unapply).optional ?? "Ex: ha")(
      UserPwd.apply,
      UserPwd.unapply
    ) orElseEither
      (string("auth_token") |@| string("clientid"))(Token.apply, Token.unapply)) ?? "Prod Config"

  val runtime = zio.Runtime.default

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

  assert(
    generateDocs(config).toTable.asGithubFlavouredMarkdown ==
      s"""
         |## Configuration Details
         |
         |
         ||FieldName|Format                         |Description|Sources|
         ||---      |---                            |---        |---    |
         ||         |[any-one-of](fielddescriptions)|           |       |
         |
         |### Field Descriptions
         |
         ||FieldName|Format                       |Description|Sources|
         ||---      |---                          |---        |---    |
         ||         |[all-of](fielddescriptions-2)|           |       |
         ||         |[all-of](fielddescriptions-1)|           |       |
         |
         |### Field Descriptions
         |
         ||FieldName|Format                           |Description                                               |Sources |
         ||---      |---                              |---                                                       |---     |
         ||usr      |primitive                        |value of type string, Example: some-user, Prod Config     |constant|
         ||pwd      |primitive                        |value of type string, optional value, sec, Prod Config    |constant|
         ||jhi      |primitive                        |value of type string, optional value, Ex: ghi, Prod Config|constant|
         ||xyz      |primitive                        |value of type string, optional value, Ex: ha, Prod Config |constant|
         ||         |[any-one-of](fielddescriptions-3)|                                                          |        |
         |
         |### Field Descriptions
         |
         ||FieldName|Format   |Description                                              |Sources |
         ||---      |---      |---                                                      |---     |
         ||abc      |primitive|value of type int, optional value, Ex: ha, Prod Config   |constant|
         ||def      |primitive|value of type string, optional value, Ex: ha, Prod Config|constant|
         |
         |### Field Descriptions
         |
         ||FieldName |Format   |Description                      |Sources |
         ||---       |---      |---                              |---     |
         ||auth_token|primitive|value of type string, Prod Config|constant|
         ||clientid  |primitive|value of type string, Prod Config|constant|
         |""".stripMargin
  )
}
