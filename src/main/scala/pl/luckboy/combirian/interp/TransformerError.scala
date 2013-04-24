package pl.luckboy.combirian.interp
import scala.util.parsing.input.Position
import pl.luckboy.combirian.AbstractError
import pl.luckboy.combirian.AbstractErrorLike

case class TransformerError(file: Option[java.io.File], pos: Position, message: String) extends AbstractErrorLike[TransformerError] with AbstractError
{
  override def withFile(file: Option[java.io.File]) =
    copy(file = file)

  override def toString =
    file.map { _.getPath }.getOrElse("<no file>") + ": " + pos.toString + ": transformer: " + message  
}