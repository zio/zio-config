package zio.config.typesafe

import Parsers._
import zio.parser.Parser

object HoconParser {

  lazy private[config] val keyValueParser: Parser[String, Char, HoconObject] =
    ws.zip(keyIdentifier)
      .zip(ws)
      .zip(keySeparator)
      .zip(hoconParser.orElse(textParserHocon.map(str => HoconObject.Text(str))))
      .map { case (k, v) => HoconObject.KeyValue(k, v) }

  lazy private[config] val hoconParser: Parser[String, Char, HoconObject] =
    ws.zip(openBracket)
      .zip(ws)
      .zip(keyValueParser.repeatWithSep0((Parser.charIn('\n').unit)))
      .map { case (k) => HoconObject.Sequence(k) }
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
  // Right(Sequence(Chunk(KeyValue(a,Sequence(Chunk(KeyValue(b,Text(c)),KeyValue(d,Text(e)),KeyValue(f,Sequence(Chunk(KeyValue(h,Sequence(Chunk(KeyValue(a,Text(c))))))))))))))
  // Flattened
  // Right(Map(Chunk(KeyName(a),KeyName(b)) -> c, Chunk(KeyName(a),KeyName(d)) -> e, Chunk(KeyName(a),KeyName(f),KeyName(h),KeyName(a)) -> c))

}
