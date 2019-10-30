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
 * Lintilla to SEC machine code translator.
 */

package lintilla

import scala.collection.mutable.ArrayBuffer

/**
  * Translator from Lintilla source programs to SEC target programs.
  */
object Translator {

  import SECDTree._
  import LintillaTree._
  import scala.collection.mutable.ListBuffer

  import scala.language.implicitConversions

  implicit private def vecToList[T](vec : Vector[T]) = vec.toList
  var control_var: String = ""
  var step_value: Int = 0
  /**
    * Return a frame that represents the SEC instructions for a Lintilla
    * program.
    */
  def translate (program : Program) : Frame =
    program match {
      case Program(exps) => translateToFrame(exps)
    }

  /**
    * Translate a sequence of Lintilla expressions and return a frame containing
    * the generated SEC code.
    */
  def translateToFrame(exps : List[Expression]) : Frame = {

    // An instruction buffer for accumulating the translated SEC code.
    val instrBuffer = new ListBuffer[Instr] ()

    // Generate an instruction by appending it to the instruction buffer.
    def gen (instr : Instr) {
      instrBuffer.append (instr)
    }

    /**
      * Translate a sequence of expressions in order, adding the SEC code
      * generated by each one to the end of the instruction buffer.
      *
      * If the first expression in the given sequence is a `let` or `fn`
      * declaration then rest of the sequence is translated into a frame, by
      * calling `translateClosureBody`. This is then used to construct an
      * `IClosure` instruction whose purpose is to bind the declared identifier
      * in the environment used when executing that frame.
      */
    def translateSeq(list : List[Expression]) {
      list match {
        case (LetDecl(IdnDef(i), exp) :: rest) =>
          translateExp(exp)
          gen(IClosure(None, List(i), translateToFrame(rest)))
          gen(ICall())
        case (FnDecl(IdnDef(n), args, _, Block(body)) :: rest) =>
          gen(IClosure(Some(n),
                       args.map({case ParamDecl(IdnDef(i), _) => i}),
                       translateToFrame(body)))
          gen(IClosure(None, List(n), translateToFrame(rest)))
          gen(ICall())
        case (exp :: rest) =>
          translateExp(exp)
          translateSeq(rest)
        case _ => ()
      }
    }

    /**
      * Translate a single Lintilla expression, adding the generated SEC code to
      * the end of the current instruction buffer.
      */
    def translateExp(exp : Expression) {
      exp match {

        // To translate a Block simply translate the list of expressions it contains

        case Block(exps) => translateSeq(exps)

        // To translate a 'print' simply translate its parameter and add a
        // print instruction.

        case PrintExp(e) =>
          translateExp(e)
          gen(IPrint())

        // To translate a function application first translate its arguments
        // then translate the funtion expression itself.

        case AppExp(fn, args) =>
          args.foreach(translateExp)
          translateExp(fn)
          gen(ICall())

        // Translate relational operators

        case EqualExp(l, r) =>
          translateExp(l)
          translateExp(r)
          gen(IEqual())

        case LessExp(l, r) =>
          translateExp(l)
          translateExp(r)
          gen(ILess())

        // Translate arithmetic operators

        case PlusExp(l, r) =>
          translateExp(l)
          translateExp(r)
          gen(IAdd())

        case MinusExp(l, r) =>
          translateExp(l)
          translateExp(r)
          gen(ISub())

        case StarExp(l, r) =>
          translateExp(l)
          translateExp(r)
          gen(IMul())

        case SlashExp(l, r) =>
          translateExp(l)
          translateExp(r)
          gen(IDiv())

        case NegExp(e) =>
          gen(IInt(0))
          translateExp(e)
          gen(ISub())

        // Translate constant expressions

        case BoolExp(b) =>
          gen(IBool(b))

        case IntExp(i) =>
          gen(IInt(i))

        // Translate an identifier use

        case IdnExp(IdnUse(n)) =>
          gen(IVar(n))

        // Translate an 'if' expression

        case IfExp(c, Block(t), Block(e)) =>
          translateExp(c)
          gen(
            IBranch(
              translateToFrame(t), 
              translateToFrame(e)
            )
          )

        // FIXME: add your translator code for logical operators, arrays and for loops here.

        // FIXME: Translate short-circuited evaluation of '&&', '||' and '~'.
        case AndExp(l, r) =>
          translateExp(l)
          gen(IBranch(translateToFrame(List(r)),
            List(IBool(false))))

        case OrExp(l, r) =>
          translateExp(l)
          gen(IBranch(List(IBool(true)),
            translateToFrame(List(r))))

        case NotExp(exp) =>
          translateExp(exp)
          gen(IBranch(List(IBool(false)),
            List(IBool(true))))

        // FIXME: Translate array creation, length, dereferencing, assignment and extension.
        case ArrayExp(t) =>
          gen(IArray())

        case LengthExp(exp) =>
          translateExp(exp)
          gen(ILength())

        case DerefExp(array, idx) =>
          translateExp(array)
          translateExp(idx)
          gen(IDeref())

        case AssignExp(DerefExp(arr,idx), right) =>
          translateExp(arr)
          translateExp(idx)
          translateExp(right)
          gen(IUpdate())

        case AppendExp(array, exp) =>
          translateExp(array)
          translateExp(exp)
          gen(IAppend())

        // FIXME: Translate 'for' loops, 'loop' and 'break' constructs.
        case ForExp(IdnDef(id),from,to,step,Block(body)) =>
          // Save original values of control_var and step_value in local variables
          val old_control_var = control_var
          val old_step_value = step_value

          // Update control_var and step_value for the loop we are currently translating
          control_var = id
          step_value = 1
          if (step.nonEmpty){
            step_value = evalIntConst(step.get) //Incase there is no provided step
          }

          translateExp(from)
          translateExp(to)

          def testLoopTermination(a : Int): List[Instr] = {
            if (a > 0) {
              List(
                IVar("_to"),
                IVar(control_var),
                ILess()
              )
            }
            else {
              List(
                IVar(control_var),
                IVar("_to"),
                ILess()
              )
            }
          }
          gen(IClosure(
            None,
            List("_from", "_to", "_break_cont"),
            List(
              IClosure(
                None,
                List("_loop_cont"),
                List(
                    IVar("_from"),
                    IVar("_loop_cont")
                )
            ),
            ICallCC(),
            IClosure(
                None,
                List(control_var, "_loop_cont"),
                testLoopTermination(step_value)
                  ++
                  List(IBranch(
                      List(
                          IVar("_break_cont"),
                          IResume()
                      ),
                      List()
                  ))
                  ++
                  translateToFrame(body)
                  ++
                  List(
                    IVar(control_var),
                    IInt(step_value),
                    IAdd(),
                    IVar("_loop_cont"),
                    IVar("_loop_cont"),
                    IResume()
                  )
              ),
            ICall()
          )
        ))
            gen(ICallCC())

          // Restore the values that control_var and step_value had on entry
          control_var = old_control_var
          step_value = old_step_value

        case BreakExp() =>
          gen(IDropAll()) // Flush the operand stack, to avoid spurious return values.
          gen(IVar("_break_cont")) // Push the break continuation onto the operand stack and...
          gen(IResume()) // ...resume it, thereby jumping to the exit point of the loop.

        case LoopExp() =>
          gen(IDropAll()) // Flush the operand stack, to avoid spurious return values.
          gen(IVar(control_var))
          gen(IInt(step_value)) // Compute the new value for the control variable...
          gen(IAdd()) // ... and leave it on the operand stack.
          gen(IVar("_loop_cont")) // Push the loop continuation onto the operand stack...
          gen(IVar("_loop_cont")) // ... twice.
          gen(IResume()) // And resume it, jumping back to the head of the loop.

        // Other cases have already been handled elsewhere
        case _ => ()

      }

    }

    // Call sequence translator
    translateSeq(exps)

    // Return generated code frame.
    instrBuffer.toList
  }

  /**
    * Evaluate an integer constant expression.
    */ 
  def evalIntConst(e : Expression): Int =
    e match {
      case IntExp(v) => v
      case NegExp(n) => -evalIntConst(n)
      case PlusExp(l, r) => evalIntConst(l) + evalIntConst(r)
      case MinusExp(l, r) => evalIntConst(l) - evalIntConst(r)
      case StarExp(l, r) => evalIntConst(l) * evalIntConst(r)
      case SlashExp(l, r) => evalIntConst(l) / evalIntConst(r)
      case _ => 0
    }

}
