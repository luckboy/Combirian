package pl.luckboy.combirian.interp
import scala.util.parsing.input.Position
import pl.luckboy.combirian.AbstractError

case class TransformerError(file: Option[java.io.File], pos: Position, message: String) extends AbstractError
{
  override def toString =
    file.map { _.getPath }.getOrElse("<no file>") + ": " + pos.toString + ": transformer: " + message
}