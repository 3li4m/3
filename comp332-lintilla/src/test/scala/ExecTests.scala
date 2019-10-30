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
 * Execution tests.
 */

package lintilla

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests to check that the execution of SEC machine code translated from
  * Lintilla source code gives the right output.
  */
@RunWith(classOf[JUnitRunner])
class ExecTests extends SemanticTests {

  import SECDTree._

  // Simple constants

  test("printing a constant integer gives the right output") {
    execTestInline("""
       |print(30)""".stripMargin, "30\n")
  }

  test("print constant integer gives the right translation") {
    targetTestInline("""
       |print(30)""".stripMargin, List(IInt(30), IPrint()))
  }

  test("printing a constant boolean gives the right output") {
    execTestInline("""
       |print(true)""".stripMargin, "true\n")
  }

  test("print constant boolean gives the right translation") {
    targetTestInline("""
       |print(true)""".stripMargin, List(IBool(true), IPrint()))
  }

  // Simple arithmetic expressions

  test("print simple addition expression") {
    execTestInline("""
       |print(30+10)""".stripMargin, "40\n")
  }

  test("print simple addition expression gives the right translation") {
    targetTestInline("""
       |print(30+10)""".stripMargin, List(IInt(30), IInt(10), IAdd(), IPrint()))
  }

  test("print simple multiplication expression") {
    execTestInline("""
       |print(6*7)""".stripMargin, "42\n")
  }

  test("print simple multiplication expression gives the right translation") {
    targetTestInline("""
       |print(6*7)""".stripMargin, List(IInt(6), IInt(7), IMul(), IPrint()))
  }

  test("print simple subtraction expression") {
    execTestInline("""
       |print(30-10)""".stripMargin, "20\n")
  }

  test("print simple subtraction expression gives the right translation") {
    targetTestInline("""
       |print(30-10)""".stripMargin, List(IInt(30), IInt(10), ISub(), IPrint()))
  }

  test("print simple division expression") {
    execTestInline("""
       |print(6/2)""".stripMargin, "3\n")
  }

  test("print simple division expression gives the right translation") {
    targetTestInline("""
       |print(6/2)""".stripMargin, List(IInt(6), IInt(2), IDiv(), IPrint()))
  }

  test("print simple negation expression") {
    execTestInline("""
       |print(-22)""".stripMargin, "-22\n")
  }

  test("print simple negation expression gives the right translation") {
    targetTestInline("""
       |print(-22)""".stripMargin, List(IInt(0), IInt(22), ISub(), IPrint()))
  }

  // Simple relation expressions

  test("print simple equality expression") {
    execTestInline("""
       |print(25=5)""".stripMargin, "false\n")
  }

  test("print simple equality expression gives the right translation") {
    targetTestInline("""
       |print(25=5)""".stripMargin, List(IInt(25), IInt(5), IEqual(), IPrint()))
  }

  test("print simple less expression") {
    execTestInline("""
       |print(7<9)""".stripMargin, "true\n")
  }

  test("print simple less expression gives the right translation") {
    targetTestInline("""
       |print(7<9)""".stripMargin, List(IInt(7), IInt(9), ILess(), IPrint()))
  }

  // More complex expressions

  test("print more complex expression") {
    execTestInline("""
       |print(10+5*6/2-21+2*-2)""".stripMargin, "0\n")
  }

  test("print more complex expression gives the right translation") {
    targetTestInline(
      """
       |print(10+5*6/2-21+2*-2)""".stripMargin,
      List(
        IInt(10),
        IInt(5),
        IInt(6),
        IMul(),
        IInt(2),
        IDiv(),
        IAdd(),
        IInt(21),
        ISub(),
        IInt(2),
        IInt(0),
        IInt(2),
        ISub(),
        IMul(),
        IAdd(),
        IPrint()
      )
    )
  }

  test("print more complex relational expression") {
    execTestInline("""
       |print((5 < 10) = (10 < 5))""".stripMargin, "false\n")
  }

  test("print more complex relational expression gives right translation") {
    targetTestInline(
      """
       |print((5 < 10) = (10 < 5))""".stripMargin,
      List(
        IInt(5),
        IInt(10),
        ILess(),
        IInt(10),
        IInt(5),
        ILess(),
        IEqual(),
        IPrint()
      )
    )
  }

  // Simple block translation

  test("block translates correctly") {
    targetTestInline(
      """
       |print 10;
       |{
       |   print 20;
       |   print 30
       |};
       |print 40""".stripMargin,
      List(
        IInt(10),
        IPrint(),
        IInt(20),
        IPrint(),
        IInt(30),
        IPrint(),
        IInt(40),
        IPrint()
      )
    )
  }

  test("nested block translates correctly") {
    targetTestInline(
      """
       |print 10;
       |{
       |   print 20;
       |   {
       |       print 30
       |   };
       |   print 40
       |};
       |print 50""".stripMargin,
      List(
        IInt(10),
        IPrint(),
        IInt(20),
        IPrint(),
        IInt(30),
        IPrint(),
        IInt(40),
        IPrint(),
        IInt(50),
        IPrint()
      )
    )
  }

  // `let` binding

  test("let binding gives right translation") {
    targetTestInline(
      """
       |let x = 20;
       |print x;
       |print x * x""".stripMargin,
      List(
        IInt(20),
        IClosure(
          None,
          List("x"),
          List(IVar("x"), IPrint(), IVar("x"), IVar("x"), IMul(), IPrint())
        ),
        ICall()
      )
    )
  }

  test("let binding body extends to end of block only") {
    targetTestInline(
      """
       |print 10;
       |{
       |    let x = 20;
       |    print x;
       |    print x * x
       |};
       |print 30""".stripMargin,
      List(
        IInt(10),
        IPrint(),
        IInt(20),
        IClosure(
          None,
          List("x"),
          List(IVar("x"), IPrint(), IVar("x"), IVar("x"), IMul(), IPrint())
        ),
        ICall(),
        IInt(30),
        IPrint()
      )
    )
  }

  test("let binds variable in rest of block") {
    execTestInline("""
       |let x = 10;
       |print x;
       |let y = x * x;
       |print y""".stripMargin, "10\n100\n")
  }

  test("let binding in block correctly shadows outer binding") {
    execTestInline(
      """
       |let x = 10;
       |print x;
       |{
       |    let x = 20;
       |    print x
       |};
       |print x""".stripMargin,
      "10\n20\n10\n"
    )
  }

  // `if` expression

  test("simple `if` expression gives right translation") {
    targetTestInline(
      """
       |if true { print 10 } else { print 20 }""".stripMargin,
      List(
        IBool(true),
        IBranch(
          List(IInt(10), IPrint()),
          List(IInt(20), IPrint())
        )
      )
    )
  }

  test("simple `if` expression evaluation (condition true)") {
    execTestInline("""
       |if (5 < 10) { print 10 } else { print 20 }""".stripMargin, "10\n")
  }

  test("simple `if` expression evaluation (condition false)") {
    execTestInline("""
       |if (5 = 10) { print 10 } else { print 20 }""".stripMargin, "20\n")
  }

  test("`let` binding correctly scoped in then block") {
    execTestInline(
      """
       |let x = 10;
       |if x = 10 { print x; let x = 20; print x }
       |     else { print x; let x = 30; print x };
       |print x""".stripMargin,
      "10\n20\n10\n"
    )
  }

  test("`let` binding correctly scoped in else block") {
    execTestInline(
      """
       |let x = 10;
       |if x = 5 { print x; let x = 20; print x }
       |    else { print x; let x = 30; print x };
       |print x""".stripMargin,
      "10\n30\n10\n"
    )
  }

  // Function binding

  test("`fn` binding gives correct translation") {
    targetTestInline(
      """
       |fn addone(n: int) -> int { n + 1 };
       |print addone;
       |print 10""".stripMargin,
      List(
        IClosure(Some("addone"), List("n"), List(IVar("n"), IInt(1), IAdd())),
        IClosure(
          None,
          List("addone"),
          List(IVar("addone"), IPrint(), IInt(10), IPrint())
        ),
        ICall()
      )
    )
  }

  test("`fn` binding extends to end of block only") {
    targetTestInline(
      """
       |let addone = 20;
       |{
       |    fn addone(n: int) -> int { n + 1 };
       |    print 10;
       |    print addone
       |};
       |print addone""".stripMargin,
      List(
        IInt(20),
        IClosure(
          None,
          List("addone"),
          List(
            IClosure(
              Some("addone"),
              List("n"),
              List(IVar("n"), IInt(1), IAdd())
            ),
            IClosure(
              None,
              List("addone"),
              List(IInt(10), IPrint(), IVar("addone"), IPrint())
            ),
            ICall(),
            IVar("addone"),
            IPrint()
          )
        ),
        ICall()
      )
    )
  }

  test("`fn` binding extends to end of block execution") {
    execTestInline(
      """
       |let addone = 20;
       |{
       |    fn addone(n: int) -> int { n + 1 };
       |    print addone
       |};
       |print addone""".stripMargin,
      "function of arguments (n)\n20\n"
    )
  }

  test("`fn` body with `let` binding translates correctly") {
    targetTestInline(
      """
        |fn local_test() {
        |    print 10;
        |    let x = 20;
        |    print x
        |};
        |print local_test""".stripMargin,
      List(
        IClosure(
          Some("local_test"),
          List(),
          List(
            IInt(10),
            IPrint(),
            IInt(20),
            IClosure(None, List("x"), List(IVar("x"), IPrint())),
            ICall()
          )
        ),
        IClosure(None, List("local_test"), List(IVar("local_test"), IPrint())),
        ICall()
      )
    )
  }

  // Function application
  test("simple function application translation") {
    targetTestInline(
      """
       |fn addone(n: int) -> int { n + 1 };
       |print addone(10)""".stripMargin,
      List(
        IClosure(Some("addone"), List("n"), List(IVar("n"), IInt(1), IAdd())),
        IClosure(
          None,
          List("addone"),
          List(IInt(10), IVar("addone"), ICall(), IPrint())
        ),
        ICall()
      )
    )
  }

  test("simple function application execution") {
    execTestInline("""
       |fn addone(n: int) -> int { n + 1 };
       |print addone(10)""".stripMargin, "11\n")
  }

  test("call a parameterless function") {
    execTestInline("""
       |fn noparam() { print 20 };
       |noparam()""".stripMargin, "20\n")
  }

  test("call a three parameter function") {
    execTestInline(
      """
       |fn threeparam(n: int, m: int, r: int) -> int {
       |    n + 10*m + 100*r
       |};
       |print threeparam(1,2,3)""".stripMargin,
      "321\n"
    )
  }

  test("curried function call") {
    execTestInline(
      """
       |fn curried(n: int) -> (fn(int, int) -> int) {
       |    fn aux(m: int, r: int) -> int {
       |        n + 10*m + 100*r
       |    };
       |    aux
       |};
       |print curried(1)(2,3)""".stripMargin,
      "321\n"
    )
  }
  // FIXME: add your execution tests of logical operators, arrays and for loops here.
  // FIXME: Tests of short-circuited evaluation of '&&', '||' and '~'.
  test("Test true && true") {
    execTestInline("""print(true && true)""".stripMargin, "true\n")
  }
  test("Test false && true") {
    execTestInline("""print(false && true)""".stripMargin, "false\n")
  }
  test("Test true && false") {
    execTestInline("""print(true && false)""".stripMargin, "false\n")
  }
  test("Test Short Circuit && ") {
    execTestInline("""print({false} && {print true; true})""".stripMargin, "false\n")
  }
  // TESTING || OR
  test("Test true || true") {
    execTestInline("""print(true || true)""".stripMargin, "true\n")
  }
  test("Test false || true") {
    execTestInline("""print(false || true)""".stripMargin, "true\n")
  }
  test("Test Short Circuit ||") {
    execTestInline("""print({true} || {print false; false})""".stripMargin, "true\n")
  }
  //TESTING ~ NOT
  test("Test ~false") {
    execTestInline("""print({~false})""".stripMargin, "true\n")
  }
  test("Test ~true") {
    execTestInline("""print({~true})""".stripMargin, "false\n")
  }
  // FIXME: Tests of execution of array operations
  // EMPPTY ARRAYS
  test("Test arrays") {
    execTestInline(
      """print(array int)""".stripMargin, "empty array\n")
  }
  test("Test arrays length") {
    execTestInline(
      """let a = array int;
        |a += 4;
        |a += 5;
        |print(length(a))""".stripMargin, "2\n")
  }
  test("Test arrays deref") {
    execTestInline(
      """let a = array int;
        |a += 4;
        |a += 5;
        |print(a ! 1)""".stripMargin, "5\n")
  }
  test("Test arrays assign") {
    execTestInline(
      """let a = array int;
        |a += 4;
        |a += 5;
        |a ! 1 := 10;
        |print(a ! 1)""".stripMargin, "10\n")
  }
  test("Test arrays append") {
    execTestInline(
      """let a = array int;
        |a += 4;
        |a += 6;
        |a += 7;
        |a += 9;
        |print(a ! 3)""".stripMargin, "9\n")
  }
  // FIXME: Tests of execution of 'for' loops, 'break' and 'loop' constructs.

  //BREAK TESTS
  test("Test For Loops Break") {
    execTestInline(
      """for j = 0 to 10 step 2 do {
        | if (j = 7) {break} else {}
        |}""".stripMargin, "")
  }
  test("Test For Loops to bool") {
    execTestInline(
      """for j = 0 to 10 step 2 do {
        | print 1;
        | break;
        | print 45
        |};
        |print (true)""".stripMargin, "1\ntrue\n")
  }
  //LOOP TESTS
  test("Test For LOOP") {
    execTestInline(
      """for j = 0 to 3 step 1 do {
        | if (j = 2) {loop} else {};
        | print j
        |}""".stripMargin, "0\n1\n3\n")
  }
  // Bigger examples.

  test("factorial example") {
    execTestFile("src/test/resources/factorial.lin", "120\n")
  }

  test("fibonacci example") {
    execTestFile("src/test/resources/fibonacci.lin", "55\n")
  }

  test("higher order example") {
    execTestFile("src/test/resources/iterate.lin", "135\n50\n27\n")

  }

  test("while loop example") {
    execTestFile(
      "src/test/resources/while.lin",
      "2\n7\n22\n67\n202\n607\n1822\n5467\n16402\n"
    )
  }

  test("snippets example") {
    execTestFile(
      "src/test/resources/snippets.lin",
      "1\n1\n2\n1\n3\n43\n6\nfunction of arguments (b)\n10\n10\n5\n15\n"
    )
  }

}
