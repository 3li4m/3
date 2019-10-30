/*
 * This file is part of COMP332 Assignment 3 2019.
 *
 * Lintilla, a simple functional programming language.
 *
 * © 2019, Dominic Verity and Anthony Sloane, Macquarie University.
 *         All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Symbol tables for the Lintilla language.
 */

package lintilla

import org.bitbucket.inkytonik.kiama._

import util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments {

  import LintillaTree._
  import util.Entity

  /**
    * A variable entity bound to the given expression.
    */
  case class Variable (exp : Expression) extends Entity

  /**
    * A named function of a given type and with a given body.
    */
  case class Function(tipe : FnType, body : Block) extends Entity

  /**
    * A function argument entity of the given type.
    */
  case class Argument (tipe : Type) extends Entity

}
