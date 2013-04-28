package pl.luckboy.combirian.interp

trait EnvironmentFactory[Env]
{
  def empty: Env
}