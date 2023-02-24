package zio.config.typesafe

import Parsers._
import zio.config.IndexedFlat.ConfigPath
import zio.parser.Parser

object HoconParser {

  lazy val sequenceParser: Parser[String, Char, HoconObject.Sequence] =
    openSquareBracket
      .zip(
        hoconParser
          .orElse(textParserHocon.map { str =>
            HoconObject.Text(str)
          })
          .repeatWithSep0(Parser.char(',').zip(ws).unit)
      )
      .zip(ws)
      .zip(closedSquareBracket)
      .map { sequence =>
        HoconObject.Sequence(sequence)
      }

  lazy private[config] val keyValueParser: Parser[String, Char, (String, HoconObject)] =
    ws.zip(keyIdentifier)
      .zip(ws)
      .zip(keySeparator)
      .zip(ws)
      .zip(sequenceParser.orElse(hoconParser).orElse(textParserHocon.map(str => HoconObject.Text(str))))

  lazy private[config] val hoconParser: Parser[String, Char, HoconObject] =
    ws.zip(openBracket)
      .zip(ws)
      .zip(keyValueParser.repeatWithSep0(Parser.charIn('\n').unit))
      .map { case (k) => HoconObject.Record(k.toMap) }
      .zip(ws)
      .zip(closedBracket)

}

object SampleTest extends App {

  val str =
    s"""
       | {
       |   a  : {
       |     b : c
       |     d : e
       |     f : {
       |       h : { a : [b, c, d] }
       |       i : [
       |         {
       |           a : b
       |         },
       |         {
       |           c : d
       |         }
       |
       |       ]
       |     }
       |   }
       | }
       |
       |""".stripMargin

  println(HoconParser.hoconParser.parseString(str).map(_.flatten.map({case (k, v) => ConfigPath.toPath(k).mkString(".") -> v})))
  // Right(HashMap(a.f.h.a[1] -> c, a.f.i[0].a -> b, a.f.h.a[2] -> d, a.b -> c, a.f.h.a[0] -> b, a.f.i[1].c -> d, a.d -> e))
  println(HoconParser.hoconParser.parseString(str))
  // Right(Record(Map(a -> Record(Map(b -> Text(c), d -> Text(e), f -> Record(Map(h -> Record(Map(a -> Text(c))))))))))

}
