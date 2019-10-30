/*
 * This file is part of COMP332 Assignment 2 2018.
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
 * Parser for the Lintilla language.
 */

package lintilla

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
  * Module containing parsers for Lintilla.
  */
class SyntaxAnalysis(positions: Positions) extends Parsers(positions) {

  import LintillaTree._

  lazy val parser: PackratParser[Program] =
    phrase(program)

  lazy val program: PackratParser[Program] =
    rep1sep(exp, ";") ^^ Program

  lazy val block: PackratParser[Block] =
    "{" ~> repsep(exp, ";") <~ "}" ^^ Block

  lazy val letdecl: PackratParser[Expression] =
    ("let" ~> idndef) ~ ("=" ~> logexp) ^^ LetDecl

  lazy val fndecl: PackratParser[FnDecl] =
    ("fn" ~> idndef) ~ ("(" ~> repsep(paramdecl, ",") <~ ")") ~
      opt("->" ~> tipe) ~ block ^^ FnDecl

  lazy val paramdecl: PackratParser[ParamDecl] =
    (idndef <~ ":") ~ tipe ^^ ParamDecl

  lazy val ifexp: PackratParser[IfExp] =
    ("if" ~> logexp) ~ block ~ ("else" ~> block) ^^ IfExp

  lazy val printexp: PackratParser[PrintExp] =
    "print" ~> logexp ^^ PrintExp
  
  lazy val assignexp: PackratParser[AssignExp] = 
    logexp ~ (":=" ~> logexp) ^^ AssignExp

  lazy val appendexp: PackratParser[AppendExp] =
    logexp ~ ("+=" ~> logexp) ^^ AppendExp

  lazy val forexp: PackratParser[ForExp] =
    ("for" ~> idndef) ~ ("=" ~> logexp) ~ ("to" ~> logexp) ~
      opt("step" ~> logexp) ~ ("do" ~> block) ^^ ForExp

  lazy val loopexp: PackratParser[LoopExp] =
    "loop" ^^^ LoopExp()

  lazy val breakexp: PackratParser[BreakExp] =
    "break" ^^^ BreakExp()

  lazy val exp: PackratParser[Expression] =
    assignexp | appendexp | logexp | ifexp | printexp | 
      forexp | loopexp | breakexp | letdecl | fndecl

  lazy val logexp: PackratParser[Expression] =
    logexp ~ ("&&" ~> pexp) ^^ AndExp |
      logexp ~ ("||" ~> pexp) ^^ OrExp |
      pexp

  lazy val pexp: PackratParser[Expression] =
    addexp ~ ("=" ~> addexp) ^^ EqualExp |
      addexp ~ ("<" ~> addexp) ^^ LessExp |
      addexp

  lazy val addexp: PackratParser[Expression] =
    addexp ~ ("+" ~> multexp) ^^ PlusExp |
      addexp ~ ("-" ~> multexp) ^^ MinusExp |
      multexp

  lazy val multexp: PackratParser[Expression] =
    multexp ~ ("*" ~> derefexp) ^^ StarExp |
      multexp ~ ("/" ~> derefexp) ^^ SlashExp |
      derefexp
  
  lazy val derefexp: PackratParser[Expression] =
    derefexp ~ ("!" ~> factor) ^^ DerefExp |
      factor

  lazy val factor: PackratParser[Expression] =
    "true" ^^^ BoolExp(true) |
      "false" ^^^ BoolExp(false) |
      integer ^^ (s => IntExp(s.toInt)) |
      "array" ~> tipe ^^ ArrayExp |
      "-" ~> factor ^^ NegExp |
      "~" ~> factor ^^ NotExp |
      "length" ~> ("(" ~> exp <~ ")") ^^ LengthExp |
      app | block | "(" ~> exp <~ ")"

  lazy val app: PackratParser[Expression] =
    app ~ ("(" ~> repsep(exp, ",") <~ ")") ^^ AppExp |
      idnuse ^^ IdnExp

  lazy val tipe: PackratParser[Type] =
    "int" ^^^ IntType() |
      "bool" ^^^ BoolType() |
      "unit" ^^^ UnitType() |
      "fn" ~> ("(" ~> repsep(tipe, ",") <~ ")") ~ ("->" ~> tipe) ^^ FnType |
      "array" ~> tipe ^^ ArrayType |
      "(" ~> tipe <~ ")"

  lazy val integer: PackratParser[String] =
    regex("[0-9]+".r)

  lazy val idndef: PackratParser[IdnDef] =
    identifier ^^ IdnDef

  lazy val idnuse: PackratParser[IdnUse] =
    identifier ^^ IdnUse

  // Parses a legal Lintilla identifier. Checks to ensure that the word parsed is
  // not a Lintilla keyword.
  lazy val identifier: PackratParser[String] =
    (not(keyword) | failure("identifier expected but keyword found")) ~>
      "[a-zA-Z][a-zA-Z0-9_]*".r

  // Parses any legal Lintilla keyword. This parser ensures that the keyword found
  // is not a prefix of an identifier. So this parser will not parse the "int" prefix
  // of "integer" as a keyword.
  lazy val keyword =
    keywords(
      "[^a-zA-Z0-9_]".r,
      List(
        "array",
        "bool",
        "break",
        "else",
        "false",
        "for",
        "fn",
        "if",
        "int",
        "let",
        "length",
        "loop",
        "print",
        "to",
        "true",
        "unit"
      )
    ) |
      failure("expecting keyword")

  // We use the character class `\R` here to match line endings, so that we correctly
  // handle all of the end-line variants in un*x, MacOSX, MS Windows, and unicode.
  override val whitespace: Parser[String] =
    """(\s|(//.*(\R|\z)))*""".r
}
