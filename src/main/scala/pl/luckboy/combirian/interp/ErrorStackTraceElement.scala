package pl.luckboy.combirian.interp
import scala.util.parsing.input.Position

case class ErrorStackTraceElement(file: Option[java.io.File], name: Option[String], pos: Position)