package pl.luckboy.combirian.interp
import scala.util.parsing.input.Position

case class ErrorStackTracePart(file: Option[java.io.File], name: Option[String], poses: Seq[Position])