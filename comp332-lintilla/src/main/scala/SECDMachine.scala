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
 * SECD machine interpreter.
 */

package lintilla

import org.bitbucket.inkytonik.kiama._

import output.PrettyPrinter
import util.Emitter
import util.OutputEmitter

/**
  * Interpreter for SECD machine instructions. The `emitter` is used to perform
  * output (default: send to standard output).
  */
class SECDMachine(emitter: Emitter = new OutputEmitter())
    extends PrettyPrinter {

  import scala.collection.mutable

  import SECDTree._
  import SECDMachine._

  /**
    * Interpret a sequence of instructions in an empty initial environment
    * and return the status at the end of the execution.
    */
  def run(prog: Frame): Status = {

    def loop(state: State): Status = {

      val State(opnds, env, code, dump) = state

      // Tracing: uncomment this to print the machine state before each instruction
      // is executed.
      // println(s"state = $state")

      code match {

        // No more instructions: if the dump contains a machine state to
        // resume then do so, otherwise return the final state and exit.
        case List() =>
          dump match {
            case Some(resume) =>
              loop(resume.copy(opnds = opnds ++ resume.opnds))
            case None => state
          }

        // Execute first instruction in the `code` list. If it doesn't
        // result in a fatal error, continue with the rest of the code.
        case i :: codetail =>
          exec(i, opnds, env, codetail, dump) match {
            case state: State => loop(state)
            case error        => error
          }
      }
    }

    // Start execution with an empty operand stack, an empty environment,
    // and a single code frame containing the program code.
    loop(State(List(), Map.empty, prog, None))

  }

  /**
    * Interpret a single instruction.
    */
  def exec(
      instr: Instr,
      opnds: Stack,
      env: Environ,
      codetail: Frame,
      dump: Option[State]
  ): Status = {

    // Tracing: uncomment these to print out each instruction or other parts
    // of the state before each instruction is executed.
    // println (s"instr = $instr")

    /**
      * Helper function for arithmetic instruction implementation. Pop two
      * integers from the operand stack, apply `op` to them and push the
      * result onto the operand stack. If the expected operands are not there,
      * raise a fatal error.
      */
    def arithmetic(opname: String, op: (Int, Int) => Int) =
      opnds match {
        case MInt(r) :: MInt(l) :: opndstail =>
          State(MInt(op(l, r)) :: opndstail, env, codetail, dump)
        case _ =>
          FatalError("int and int expected in " + opname)
      }

    // Dispatch the instruction to its implementation

    instr match {

      case IAdd() => arithmetic("add", _ + _)

      case IAppend() =>
        opnds match {
          case v :: MArray(c) :: opndstail =>
            c += v
            State(opndstail, env, codetail, dump)
          case _ => FatalError("value and array expected for array extend")
        }

      case IArray() => State(MArray(mutable.ArrayBuffer()) :: opnds, env, codetail, dump)

      case IBool(value) => State(MBool(value) :: opnds, env, codetail, dump)

      case IBranch(left, right) =>
        opnds match {
          case MBool(value) :: opndstail =>
            State(
              opndstail,
              env,
              (if (value) left else right) ++ codetail,
              dump
            )
          case _ =>
            FatalError("bool expected for branch")
        }

      case ICall() =>
        opnds match {
          case MClosure(argNames, body, context) :: opndstail
              if (opndstail.length >= argNames.length) =>
            val cont =
              State(opndstail.drop(argNames.length), env, codetail, dump)
            State(Nil, context ++ (argNames zip opndstail), body, Some(cont))
          case _ =>
            FatalError(
              "closure and actual parameters expected for call"
            )
        }

      case ICallCC() =>
        opnds match {
          case MClosure(contName :: argNames, body, context) :: opndstail
              if (opndstail.length >= argNames.length) =>
            val cont =
              State(opndstail.drop(argNames.length), env, codetail, dump)
            State(
              Nil,
              context + (contName -> MContinuation(cont)) ++ (argNames zip opndstail),
              body,
              Some(cont)
            )
          case _ =>
            FatalError(
              "closure with at least one formal parameter and actual parameters expected for callcc"
            )
        }

      case IClosure(optFunName, argNames, body) =>
        val closure = MClosure(argNames.reverse, body, env)
        optFunName match {
          case Some(funName) =>
            closure.env = closure.env + ((funName -> closure))
          case None =>
          // Do nothing
        }
        State(closure :: opnds, env, codetail, dump)

      case IDeref() =>
        opnds match {
          case MInt(i) :: MArray(c) :: opndstail =>
            if (i >= 0 && i < c.length)
              State(c(i) :: opndstail, env, codetail, dump)
            else 
              FatalError("array index out of bounds")
          case _ => FatalError("int and array expected in deref")
        }

      case IDiv() =>
        opnds match {
          case MInt(r) :: MInt(l) :: opndstail =>
            if (r == 0)
              FatalError("division by zero")
            else
              State(MInt(l / r) :: opndstail, env, codetail, dump)
          case _ =>
            FatalError("int and int expected in div")
        }
      
      case IDropAll() => State(List(), env, codetail, dump)

      case IEqual() =>
        opnds match {
          case MBool(r) :: MBool(l) :: opndstail =>
            State(MBool(l == r) :: opndstail, env, codetail, dump)
          case MInt(r) :: MInt(l) :: opndstail =>
            State(MBool(l == r) :: opndstail, env, codetail, dump)
          case _ =>
            FatalError("int and int, or bool and bool expected in equal")
        }

      case IInt(value) =>
        State(MInt(value) :: opnds, env, codetail, dump)

      case ILess() =>
        opnds match {
          case MInt(r) :: MInt(l) :: opndstail =>
            State(MBool(l < r) :: opndstail, env, codetail, dump)
          case _ => FatalError("int and int expected for less")
        }

      case ILength() =>
        opnds match {
          case MArray(c) :: opndstail =>
            State(MInt(c.length) :: opndstail, env, codetail, dump)
          case _ => FatalError("array expected for length")
        }

      case IMul() =>
        arithmetic("mul", _ * _)

      case IPrint() =>
        opnds match {
          case value :: opndstail =>
            emitter.emitln(value)
            State(opndstail, env, codetail, dump)
          case _ => FatalError("value expected for print")
        }

      case IResume() =>
        opnds match {
          case MContinuation(resume) :: opndstail =>
            resume.copy(opnds = opndstail ++ resume.opnds)
          case _ => FatalError("continuation expected for resume")
        }

      case ISub() => arithmetic("sub", _ - _)

      case IUpdate() =>
        opnds match {
          case v :: MInt(i) :: MArray(c) :: opndstail =>
            if (i >= 0 && i < c.length) {
              c(i) = v
              State(opndstail, env, codetail, dump)
            } else
              FatalError("array index out of bounds")
          case _ => FatalError("value, int and array expected for update")
        }

      case IVar(name) =>
        env.get(name) match {
          case Some(value) => State(value :: opnds, env, codetail, dump)
          case None        => FatalError("unknown variable '" + name + "'")
        }
    }
  }
}

object SECDMachine {

  import SECDTree._

  type Stack = List[MValue]

  /**
    * The status of the current interpreter execution.
    */
  sealed abstract class Status

  /**
    * Runtime state:
    * - a stack of operands to be operated on by the instructions,
    * - an environment that binds free variables,
    * - a list of instructions that are still to be executed, and
    * - a dump containing a machine state to resume when the current code
    * sequence is empty.
    */
  case class State(
      opnds: Stack,
      env: Environ,
      code: Frame,
      dump: Option[State]
  ) extends Status

  /**
    * Runtime error: a fatal error has occurred, described by message.
    */
  case class FatalError(message: String) extends Status
}
