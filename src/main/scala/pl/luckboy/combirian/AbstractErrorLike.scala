package pl.luckboy.combirian

trait AbstractErrorLike[+This]
{
  def withFile(file: Option[java.io.File]): This
}