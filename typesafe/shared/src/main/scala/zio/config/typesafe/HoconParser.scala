package zio.config.typesafe

import Parsers._
import zio.parser.Parser

object HoconParser {

  lazy private[config] val keyValueParser: Parser[String, Char, (String, HoconObject)] =
    ws.zip(keyIdentifier)
      .zip(ws)
      .zip(keySeparator)
      .zip(hoconParser.orElse(textParserHocon.map(str => HoconObject.Text(str))))

  lazy private[config] val hoconParser: Parser[String, Char, HoconObject] =
    ws.zip(openBracket)
      .zip(ws)
      .zip(keyValueParser.repeatWithSep0((Parser.charIn('\n').unit)))
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
       |       h : { a : c }
       |     }
       |   }
       | }
       |
       |""".stripMargin

  println(HoconParser.hoconParser.parseString(str).map(_.flatten))
  println(HoconParser.hoconParser.parseString(str))
  // Right(Map(Chunk(KeyName(a),KeyName(b)) -> c, Chunk(KeyName(a),KeyName(d)) -> e, Chunk(KeyName(a),KeyName(f),KeyName(h),KeyName(a)) -> c))
  // Right(Record(Map(a -> Record(Map(b -> Text(c), d -> Text(e), f -> Record(Map(h -> Record(Map(a -> Text(c))))))))))

}
