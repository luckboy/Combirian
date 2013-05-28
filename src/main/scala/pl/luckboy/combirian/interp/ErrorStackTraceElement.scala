/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp
import scala.util.parsing.input.Position

case class ErrorStackTraceElement(file: Option[java.io.File], name: Option[String], pos: Position, isFinal: Boolean)
{
  override def toString = 
    "\t" + file.map { _.getPath }.getOrElse("<no file>") + ": " + name.getOrElse("<lambda>") + ": " + pos
}
