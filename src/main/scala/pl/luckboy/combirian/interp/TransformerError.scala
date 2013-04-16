package pl.luckboy.combirian.interp
import scala.util.parsing.input.Position

case class TransformerError(file: Option[java.io.File], pos: Position, message: String)