package zio.config

import zio.config.Config._
import zio.config.helpers._
import zio.test._
import zio.test.Assertion._

object SequenceRoundtripTest
    extends BaseSpec(
      suite("sequence round trip")(
        testM("optional write") {
          checkM(genOverallConfig) {
            p =>
              val cId: String => ConfigDescriptor[Id] = string(_).xmap(Id)(_.value)

              val config =
                ConfigDescriptor.sequence(
                  p.toList.map(
                    prefix => (cId(prefix._1).optional |@| cId(prefix._1))(OverallConfig.apply, OverallConfig.unapply)
                  )
                )

              val readAndWrite =
                for {
                  result  <- read(config)
                  written <- write(config).provide(result).either
                } yield written

              val actual = readAndWrite.provide(mapSource(p)).map(_.fold(_ => Nil, _.toList.sortBy(_._1)))

              assertM(actual, equalTo(p.toList.sortBy(_._1)))
          }
        }
      )
    )
