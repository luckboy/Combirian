/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
