package zio.config.yaml.generators

import zio.test.Gen

sealed trait WhiteSpacedBracket {

  def print: String =
    this match {
      case WhiteSpacedBracket.Open(startSpace, stopSpace) =>
        Printer.print(startSpace.print, "<", stopSpace.print)
      case WhiteSpacedBracket.Closed(startSpace, stopSpace) =>
        Printer.print(startSpace.print, ">", stopSpace.print)
      case WhiteSpacedBracket.SlashedOpen(startSpace, stopSpace) =>
        Printer.print(startSpace.print, "</", stopSpace.print)
    }

}

object WhiteSpacedBracket {
  case class Open(startSpace: Space, stopSpace: Space) extends WhiteSpacedBracket
  case class Closed(startSpace: Space, stopSpace: Space) extends WhiteSpacedBracket
  case class SlashedOpen(StartSpace: Space, stopSpace: Space) extends WhiteSpacedBracket

  def open: Gen[Any, WhiteSpacedBracket.Open] =
    for {
      start <- Space.gen
      stop <- Space.gen
    } yield Open(start, stop)

  def closed: Gen[Any, WhiteSpacedBracket.Closed] =
    for {
      start <- Space.gen
      stop <- Space.gen
    } yield Closed(start, stop)

  def slashedOpen: Gen[Any, WhiteSpacedBracket.SlashedOpen] =
    for {
      start <- Space.gen
      stop <- Space.gen
    } yield SlashedOpen(start, stop)
}