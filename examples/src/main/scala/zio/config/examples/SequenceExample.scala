package zio.config.examples

import zio.DefaultRuntime
import zio.config._, Config._

/**
 * This is only an example of a working pattern that reads the environment variables to form a `List[A]`,
 * to show how the combinator `Config.collectAll` (Sequence)  can be helpful.
 *
 * This is not showing a standard pattern that user has to follow. It is up to the user to design the pattern
 * of key value pairs, and use the right combinators in the library to retrieve it purely and safely integrated with ZIO.
 */
final case class Variables(variable1: Int, variable2: Option[Int])

object SequenceExample extends App {
  val listOfConfig: List[ConfigDescriptor[Variables]] =
    List("GROUP1", "GROUP2", "GROUP3", "GROUP4")
      .map(
        group =>
          (int(s"${group}_VARIABLE1") |@| int(s"${group}_VARIABLE2").optional)(Variables.apply, Variables.unapply)
      )

  val configOfList: ConfigDescriptor[List[Variables]] =
    ConfigDescriptor.collectAll(listOfConfig)

  val map =
    Map(
      "GROUP1_VARIABLE1" -> "1",
      "GROUP1_VARIABLE2" -> "2",
      "GROUP2_VARIABLE1" -> "3",
      "GROUP2_VARIABLE2" -> "4",
      "GROUP3_VARIABLE1" -> "5",
      "GROUP3_VARIABLE2" -> "6",
      "GROUP4_VARIABLE1" -> "7"
    )

  val runtime = new DefaultRuntime {}

  val result  = runtime.unsafeRun(read(configOfList).provide(mapSource(map)))
  val written = runtime.unsafeRun(write(configOfList).run.provide(result).either)

  assert(result == List(Variables(7, None), Variables(5, Some(6)), Variables(3, Some(4)), Variables(1, Some(2))))
  assert(written.fold(_ => Nil, _.toList.sortBy(_._1)) == map.toList.sortBy(_._1))
}
