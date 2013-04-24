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