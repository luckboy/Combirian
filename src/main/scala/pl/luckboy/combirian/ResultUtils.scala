/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian

object ResultUtils
{
  def zipResults[E <: AbstractError, T, U](res1: Either[Seq[E], T], res2: Either[Seq[E], U]) =
    (res1, res2) match {
      case (Right(x), Right(y))       => Right(x, y)
      case (Left(errs1), Left(errs2)) => Left(errs1 ++ errs2)
      case (Left(errs), _)            => Left(errs)
      case (_, Left(errs))            => Left(errs)
    }

  def resultForFile[E <: AbstractErrorLike[E], T](res: Either[Seq[E], T], file: java.io.File) =
    res match {
      case Right(x)   => Right(x)
      case Left(errs) => Left(errs.map { _.withFile(Some(file)) })
    }
}
