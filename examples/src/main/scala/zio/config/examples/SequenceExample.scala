package zio.config.examples

import zio.DefaultRuntime
import zio.config._

final case class Variables(variable1: Int, variable2: Option[Int])

/**
 * This is only an example of a working pattern that reads the environment variables to form a `List[A]`,
 * to show how the combinator `Config.sequence` can be helpful.
 *
 * This is not showing a standard pattern that user has to follow. It is up to the user to design the pattern
 * of key value pairs, and use the right combinators in the library to retrieve it purely and safely integrated with ZIO.
 */
object SequenceExample extends App {
  val listOfConfig: List[Config[Variables]] =
    List("GROUP1", "GROUP2", "GROUP3", "GROUP4")
      .map(
        group => (int(s"${group}_VARIABLE1") <*> opt(int(s"${group}_VARIABLE2")))(Variables.apply, Variables.unapply)
      )

  val configOfList: Config[List[Variables]] =
    Config.sequence(listOfConfig)

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

  val result  = runtime.unsafeRun(read(configOfList).run.provide(mapSource(map)))
  val written = runtime.unsafeRun(write(configOfList).run.provide(result._2).either)

  assert(result._2 == List(Variables(7, None), Variables(5, Some(6)), Variables(3, Some(4)), Variables(1, Some(2))))
  assert(written.map(_.toList.sortBy(_._1)) == Right(map.toList.sortBy(_._1)))
}
