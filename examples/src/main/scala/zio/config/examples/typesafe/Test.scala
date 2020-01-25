package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor.{ list, nested }
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon

final case class Blue(region: Option[String])

object Blue {
  val desc = description[Blue]
}

object Mainx extends App {
  val hocconSourceList =
    hocon(
      Right(
        """
        mylist = [{
         region: v
        },
        {
            region: x
        }]
       """
      )
    )

  val runtime = new DefaultRuntime {}

  val result = runtime.unsafeRun(read(nested("mylist")(list(Blue.desc)) from (hocconSourceList)).either)

  println(result)
}
