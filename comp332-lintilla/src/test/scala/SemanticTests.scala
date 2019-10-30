/*
 * This file is part of COMP332 Assignment 3 2019.
 *
 * Lintilla, a simple functional programming language.
 *
 * © 2017, Dominic Verity, Macquarie University, All rights reserved.
 * © 2013, Anthony Sloane, Macquarie University, All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Semantic analysis test support
 */

package lintilla

import org.bitbucket.inkytonik.kiama._

import util.ParseTests
import util.Messaging
import lintilla.SECDMachine.FatalError

/**
  * Support code for semantic test suites.
  */
class SemanticTests extends ParseTests with Messaging {

  import LintillaTree._
  import SECDTree._
  import Translator._
  import parsing.{Error, Failure, Success}
  import util.Source
  import util.StringSource
  import util.FileSource
  import util.Messaging.Messages
  import util.StringEmitter

  val parsers = new SyntaxAnalysis (positions)

  /**
    * Parse test input from a source and, if the parse succeeds with no 
    * input left, return the program tree. If the parse fails, 
    * fail the test.
    */
  def parseProgram(src : Source) : Program =
    parsers.parse(parsers.parser, src) match {
      case Success(r, in) =>
        if (!in.atEnd) fail ("input remaining at " + in.pos)
        r
      // Parsing failed, so report it
            case f : Error =>
                fail ("parse error: " + f.message)
            case f : Failure =>
                fail ("parse failure: " + f.message)
    }

  /**
    * Parse some test input and run the semantic analyser over the resulting
    * tree (if the parse succeeds).
    */
  def semanticTest(src : Source) : Messages = {
    val prog = parseProgram(src)
    val tree = new SourceTree(prog)
    val analysis = new SemanticAnalysis(tree)
    assert(analysis.envout(prog).length > 0, "scope error, global scope missing")
    assert(analysis.envout(prog).length === 1, "scope error, unclosed local scope")
    val messages = analysis.errors.sorted
    // println (messages)
    messages
  }

  /**
    * Parse some test input from a string and run the semantic analyser.
    */
  def semanticTestInline(src : String) : Messages =
    semanticTest(StringSource(src))

  /**
    * Parse some test input from a string and run the semantic analyser.
    */
  def semanticTestFile(filename : String) : Messages =
    semanticTest(FileSource(filename))

  /**
    * Parse some test input, run the semantic analyser over the resulting
    * tree, translate to SEC code, and execute.
    */
  def execTest(src : Source, expected : String) {
    val prog = parseProgram(src)
    val tree = new SourceTree(prog)
    val analysis = new SemanticAnalysis(tree)

    assert(analysis.envout(prog).length > 0, "scope error, global scope missing")
    assert(analysis.envout(prog).length === 1, "scope error, unclosed local scope")
    assert(analysis.errors.length === 0, "semantic analysis error(s)")

    val translated = translate(prog)
    val emitter = new StringEmitter()
    val machine = new SECDMachine(emitter)

    machine.run(translated) match {
      case FatalError(message) => emitter.emitln("FatalError: " + message)
      case _ => ()
    }

    emitter.result() shouldBe expected
   }

  /**
    * Run execution test with test input from a string and check the output.
    */
  def execTestInline(src : String, expected : String) {
    execTest(StringSource(src), expected)
  }

  /**
    * Run execution test with test input from a file and check the output.
    */
  def execTestFile(filename : String, expected : String) {
    execTest(FileSource(filename), expected)
  }

  /**
    * Parse some test input, run the semantic analyser over the resulting
    * tree and translate to SEC code.
    */
  def targetTest(src : Source, expected : Frame) {
    val prog = parseProgram(src)
    val tree = new SourceTree(prog)
    val analysis = new SemanticAnalysis(tree)

    assert(analysis.envout(prog).length > 0, "scope error, global scope missing")
    assert(analysis.envout(prog).length === 1, "scope error, unclosed local scope")
    assert(analysis.errors.length === 0, "semantic analysis error(s)")

    translate(prog) shouldBe expected
   }

  /**
    * Run target test with test input from a string and check the output.
    */
  def targetTestInline(src : String, expected : Frame) {
    targetTest(StringSource(src), expected)
  }

  /**
    * Run target test with test input from a file and check the output.
    */
  def targetTestFile(filename : String, expected : Frame) {
    targetTest(FileSource(filename), expected)
  }

  /**
    * Assert that a message was produced at a given position.
    */
  def assertMessage (
    messages : Messages, index : Int,
    line : Int, column : Int, msg : String) : Unit = {
    val m = messages(index)
    m.label shouldBe msg
    positions.getStart (m.value) match {
      case Some (posn) =>
        posn.line shouldBe line
        posn.column shouldBe column
      case _ =>
        fail ("no position for message value")
    }
  }

  // Most of the tests of type attribution work by triggering a type error
  // and then using the reported types in the resulting message to verify
  // either an expected or inferred type.

  // The following assertion methods support this approach, by using
  // a regular expression to analyse a type error message and extract
  // the reported expected and inferred types.

  /**
    * Regexp to analyse type error messages.
    */
  val TypeErrorRegex =
    """^type error, expecting ('.+?'( or '.+?')*) found '(.+?)'$""".r
  val typeRegex = """'(.+?)'""".r

  /**
    * Assert that a message is a type error with specified list of
    * expected types.
    */
  def assertTypeErrorWithExpectedType (
    messages : Messages, index : Int,
    line : Int, column : Int, tipes : Set[String]) : Unit = {
    val m = messages(index)

    m.label match {
      case TypeErrorRegex(exp,_,_) =>
        val found =
          (for(m <- typeRegex findAllMatchIn exp) yield m.group(1)).toSet
        found shouldBe tipes
      case _ => fail("not a type error message")
    }

    positions.getStart (m.value) match {
      case Some (posn) =>
        posn.line shouldBe line
        posn.column shouldBe column
      case _ =>
        fail ("no position for message value")
    }
  }

  /**
    * Assert that a message is a type error with specified 
    * inferred type.
    */
  def assertTypeErrorWithInferredType (
    messages : Messages, index : Int,
    line : Int, column : Int, tipe : String) : Unit = {
    val m = messages(index)

    m.label match {
      case TypeErrorRegex(_,_,inf) => inf shouldBe tipe
      case _ => fail("not a type error message")
    }

    positions.getStart (m.value) match {
      case Some (posn) =>
        posn.line shouldBe line
        posn.column shouldBe column
      case _ =>
        fail ("no position for message value")
    }
  }
}
