/*
 * This file is part of COMP332 Assignment 3 2019.
 *
 * Lintilla, a simple functional programming language.
 *
 * © 2019, Dominic Verity, Macquarie University, All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Name analysis tests.
 */

package lintilla

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests that check that the name analysis works correctly.  I.e., we run
  * the checking process over some input and make sure that the expected
  * errors were produced (and only them).
  */
@RunWith(classOf[JUnitRunner])
class TypeAnalysisTests extends SemanticTests {

  test ("the left parameter to '&&' must be of type 'bool'") {
    val messages =
      semanticTestInline ("""
             |let b = 1 && true
             |""".stripMargin)
    assert (messages.length === 1, "expecting one error")
    assertMessage (messages, 0, 2, 9, "type error, expecting 'bool' found 'int'")
  }

  test ("the right parameter to '&&' must be of type 'bool'") {
    val messages =
      semanticTestInline ("""
             |let b = true && 1
             |""".stripMargin)
    assert (messages.length === 1, "expecting one error")
    assertMessage (messages, 0, 2, 17, "type error, expecting 'bool' found 'int'")
  }

  test ("the left parameter to '||' must be of type 'bool'") {
    val messages =
      semanticTestInline ("""
             |let b = 1 || true
             |""".stripMargin)
    assert (messages.length === 1, "expecting one error")
    assertMessage (messages, 0, 2, 9, "type error, expecting 'bool' found 'int'")
  }

  test ("the right parameter to '||' must be of type 'bool'") {
    val messages =
      semanticTestInline ("""
             |let b = true || 1
             |""".stripMargin)
    assert (messages.length === 1, "expecting one error")
    assertMessage (messages, 0, 2, 17, "type error, expecting 'bool' found 'int'")
  }

  test ("the parameter to a '~' must be of type 'bool'") {
    val messages =
      semanticTestInline (
        """|let v = ~1
           |""".stripMargin
      )
      assert (messages.length === 1, "expecting one error")
      assertMessage (messages, 0, 1, 10, "type error, expecting 'bool' found 'int'")
    }

  test ("an '&&' expression has type 'bool'") {
    val messages =
      semanticTestInline ("""
             |let v = 1 + (true && false)
             |""".stripMargin)
    assert (messages.length === 1, "expecting one error")
    assertMessage (messages, 0, 2, 14, "type error, expecting 'int' found 'bool'")
  }

  test ("an '||' expression has type 'bool'") {
    val messages =
      semanticTestInline ("""
             |let v = 1 + (true || false)
             |""".stripMargin)
    assert (messages.length === 1, "expecting one error")
    assertMessage (messages, 0, 2, 14, "type error, expecting 'int' found 'bool'")
  }

  test ("a '~' expression has type 'bool'") {
    val messages =
      semanticTestInline(
        """|let v = 1 + ~true
           |""".stripMargin
      )
    assert(messages.length === 1, "expecting one error")
    assertMessage (messages, 0, 1, 13, "type error, expecting 'int' found 'bool'")
  }

  // Tests of the typing of array operations.

  test("an array may always be dereferenced") {
    val messages =
      semanticTestInline(
        """|
           |let x = array bool ! 0;
           |let y = array int ! 10;
           |fn f() { print 20 };
           |let z = array fn()->unit ! y;
           |let w = array array int ! 0 ! 0
           |""".stripMargin
      )
    assert(messages.length === 0, "expecting no errors")
  }

  test("only an array can be dereferenced") {
    val messages =
      semanticTestInline(
        """|
           |let x = true ! 0;
           |let y = 10 ! 0;
           |fn f() { print 20 };
           |let z = f ! 0
           |""".stripMargin
      )
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 2, 9, "type error, expecting 'array ?' found 'bool'")
    assertMessage(messages, 1, 3, 9, "type error, expecting 'array ?' found 'int'")
    assertMessage(messages, 2, 5, 9, "type error, expecting 'array ?' found 'fn()->unit'")
  }

  test("check the type of a dereferenced array") {
    val messages =
      semanticTestInline(
        """|
           |let x = array int;
           |print x ! 10 + 20;
           |print x ! 0 && false;
           |let y = array bool;
           |print y ! 10 || true;
           |print y ! (x ! 0) + 1;
           |let z = array array bool;
           |print z ! 0 ! 0 && true;
           |print z ! 0 + 10;
           |let w = array fn(int)->int;
           |let f = w ! 0;
           |print f(10) + 10;
           |print f && false
           |""".stripMargin
      )
    assert(messages.length === 4, "expecting four errors")
    assertMessage(messages, 0, 4, 7, "type error, expecting 'bool' found 'int'")
    assertMessage(messages, 1, 7, 7, "type error, expecting 'int' found 'bool'")
    assertMessage(messages, 2, 10, 7, "type error, expecting 'int' found 'array bool'")
    assertMessage(messages, 3, 14, 7, "type error, expecting 'bool' found 'fn(int)->int'")
  }

  test("check typing of append operation '+='") {
    val messages =
      semanticTestInline(
        """|
           |let v = array int;
           |v += 10;
           |v += true;
           |10 += 10;
           |true += false
           |""".stripMargin
      )
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 4, 6, "type error, expecting 'int' found 'bool'")
    assertMessage(messages, 1, 5, 1, "type error, expecting 'array ?' found 'int'")
    assertMessage(messages, 2, 6, 1, "type error, expecting 'array ?' found 'bool'")
  }

  test("check typing of assignment ':='") {
    val messages =
      semanticTestInline(
        """|
           |let v = array bool;
           |v += true;
           |v!0 := false;
           |v!0 := 10;
           |v := 20;
           |fn f() { print 20 };
           |fn g(x: int)->int { x * x }; 
           |let w = array fn()->unit;
           |w!0 := f;
           |w!1 := g
           |""".stripMargin
      )
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 5, 8, "type error, expecting 'bool' found 'int'")
    assertMessage(messages, 1, 6, 1, 
      "type error, left hand side of an assignment must refer to an array entry")
    assertMessage(messages, 2, 11, 8, "type error, expecting 'fn()->unit' found 'fn(int)->int'")
  }

  test("check typing of from, to an step expressions in a 'for' loops") {
    val messages =
      semanticTestInline(
        """|fn f() {};
           |for x = false to array int step f do { }
           |""".stripMargin
      )
    assert(messages.length === 4, "expecting four messages")
    assertMessage(messages, 0, 2, 9, "type error, expecting 'int' found 'bool'")
    assertMessage(messages, 1, 2, 18, "type error, expecting 'int' found 'array int'")
    assertMessage(messages, 3, 2, 33, "type error, expecting 'int' found 'fn()->unit'")
    assertMessage(messages, 2, 2, 33, "step expression of a 'for' loop must be a non-zero constant")
  }

  test("check that the body of a 'for' loop must have type 'unit'") {
    val messages =
      semanticTestInline(
        """|for i = 1 to 10 do { i * i }
           |""".stripMargin
      )
    assert(messages.length === 1, "expecting one message")
    assertMessage(messages, 0, 1, 20, "type error, expecting 'unit' found 'int'")
  }

  test("check that 'loop' may only occur in the body of a 'for'") {
    val messages =
      semanticTestInline(
        """|loop;
           |for x = 1 to 10 do {
           |  fn f() { 
           |    loop;
           |    for y = 1 to 10 do {
           |      loop
           |    }
           |  };
           |  loop
           |}
           |""".stripMargin
      )
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 1, 1, "'loop' must occur in the body of a 'for' loop")
    assertMessage(messages, 1, 4, 5, "'loop' must occur in the body of a 'for' loop")
  }

  test("check that 'break' may only occur in the body of a 'for'") {
    val messages =
      semanticTestInline(
        """|break;
           |for x = 1 to 10 do {
           |  fn f() { 
           |    break;
           |    for y = 1 to 10 do {
           |      break
           |    }
           |  };
           |  break
           |}
           |""".stripMargin
      )
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 1, 1, "'break' must occur in the body of a 'for' loop")
    assertMessage(messages, 1, 4, 5, "'break' must occur in the body of a 'for' loop")
  }

}
