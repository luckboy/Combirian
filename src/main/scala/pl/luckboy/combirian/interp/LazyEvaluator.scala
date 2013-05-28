/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp

object LazyEvaluator extends Evaluator[Environment]
{
  override def valueFromTerm(term: Term)(env: Environment) = {
    term match {
      case _: App | _: Let =>
        val newEnv = env.clone()
        LazyValue(eval(term)(newEnv).force)
      case _               =>
        eval(term)(env)
    }
  }
}
