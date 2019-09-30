package zio.config.examples

import zio.DefaultRuntime
import zio.config._

final case class Variables(variable1: Int, variable2: Option[Int])


object ListExample extends App {
  val listOfConfig: List[Config[Variables]] =
    (0 to 3).toList.map(t => (int(s"${t}_VARIABLE1") |@| int(s"${t}_VARIABLE2").optional)(Variables.apply, Variables.unapply))

  val configOfList: Config[List[Variables]] =
    Config.sequence(listOfConfig)

  val map = mapSource(
    Map(
      "0_VARIABLE1" -> "1",
      "0_VARIABLE2" -> "2",
      "1_VARIABLE1" -> "3",
      "1_VARIABLE2" -> "4",
      "2_VARIABLE1" -> "5",
      "2_VARIABLE2" -> "6",
      "3_VARIABLE1" -> "7",
    )
  )

  val runtime = new DefaultRuntime {}

  val value = runtime.unsafeRun(read(configOfList).run.provide(map))

  assert(value._2 == List(Variables(7, None), Variables(5, Some(6)), Variables(3, Some(4)), Variables(1, Some(2))))
}
