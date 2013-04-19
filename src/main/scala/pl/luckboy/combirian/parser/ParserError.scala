package pl.luckboy.combirian.parser
import scala.util.parsing.input.Position
import pl.luckboy.combirian.AbstractError

case class ParserError(file: Option[java.io.File], pos: Position, message: String) extends AbstractError