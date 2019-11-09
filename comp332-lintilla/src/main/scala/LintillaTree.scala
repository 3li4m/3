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
 * Source program tree definition.
 */

package lintilla

/**
  * Module containing tree structures for representing Lintilla programs.
  */
object LintillaTree {

  import org.bitbucket.inkytonik.kiama.relation.Tree

  /**
    * A relational tree to handle access to parent and sibling nodes.
    */
  type SourceTree = Tree[SourceNode, Program]

  /**
    * The common supertype of all source tree nodes.
    */
  sealed abstract class SourceNode extends Product

  /**
    * A Lintilla program is a list of expressions. The parser ensures that
    * this list is not empty.
    */
  case class Program(exps: Vector[Expression]) extends SourceNode

  /**
    * Common superclass of expressions.
    */
  sealed abstract class Expression extends SourceNode

  /**
    * A `let` expression declares a new immutable variable and initialises it
    * with the value computed by evaluating an associated expression. The scope
    * of the name it introduces extends from the point of the defining `let` to
    * the end of the enclosing block.
    */
  case class LetDecl(name: IdnDef, exp: Expression) extends Expression

  /**
    * A `fn` expression declares a new named function. The scope of the name it
    * binds extends throughout the enclosing block, thereby allowing us to define
    * (mutually) recursive functions.
    */
  case class FnDecl(
      name: IdnDef,
      args: Vector[ParamDecl],
      optRet: Option[Type],
      body: Block
  ) extends Expression

  /**
    * A single parameter declaration.
    */
  case class ParamDecl(idn: IdnDef, tipe: Type) extends SourceNode

  /**
    * A block comprising a non-empty list of statements.
    */
  case class Block(stmts: Vector[Expression]) extends Expression

  /**
    * Command to print the result of evaluating a given expression.
    */
  case class PrintExp(exp: Expression) extends Expression

  /**
    * For loop.
    */
  case class ForExp(
      idn: IdnDef,
      from: Expression,
      to: Expression,
      step: Option[Expression],
      body: Block
  ) extends Expression

  /**
    * Loop command, abort the current iteration of the smallest enclosing for loop
    * and commence the next iteration.
    */
  case class LoopExp() extends Expression

  /**
    * Break command, early exit from the smallest enclosing for loop.
    */
  case class BreakExp() extends Expression

  /**
    * Function application applies the left expression to the right expression.
    */
  case class AppExp(fn: Expression, args: Vector[Expression]) extends Expression

  /**
    * Make a 1-d array of mutable cells that can contain values of a specified type.
    * The array created initially contains no entries.
    */
  case class ArrayExp(tipe: Type) extends Expression

  /**
    * Dereference, return the value stored in a specified cell of an array.
    * If used on the lhs of an assignment, specified the cell of an array to
    * store a new value into.
    */
  case class DerefExp(array: Expression, index: Expression) extends Expression

  /**
    * Array append, add a extra value to the end of an array.
    */
  case class AppendExp(array: Expression, exp: Expression) extends Expression

  /**
    * Return the length of an array.
    */
  case class LengthExp(array: Expression) extends Expression

  /**
    * Assignment, store a new value into a cell of an array. The top level
    * operator of the expression on the left must be an array dereference.
    */
  case class AssignExp(left: Expression, right: Expression) extends Expression

  /**
    * Logical and expression, implemented using short-circuiting.
    */
  case class AndExp(left: Expression, right: Expression) extends Expression

  /**
    * Logical or expression, implemented using short-circuiting.
    */
  case class  OrExp(left: Expression, right: Expression) extends Expression

  /**
    * Logical not expression.
    */
  case class NotExp(exp: Expression) extends Expression 

  /**
    * Equality expression compares the left and right expressions for equality.
    */
  case class EqualExp(left: Expression, right: Expression) extends Expression

  /**
    * Less than expression compares the left and right numeric expressions
    *  for less-than order.
    */
  case class LessExp(left: Expression, right: Expression) extends Expression

  /**
    * Addition expression.
    */
  case class PlusExp(left: Expression, right: Expression) extends Expression

  /**
    * Subtraction expression.
    */
  case class MinusExp(left: Expression, right: Expression) extends Expression

  /**
    * Multiplication expression.
    */
  case class StarExp(left: Expression, right: Expression) extends Expression

  /**
    * Integer division expression.
    */
  case class SlashExp(left: Expression, right: Expression) extends Expression

  /**
    * Integer (unary) negation.
    */
  case class NegExp(exp: Expression) extends Expression

  /**
    * Boolean constant expression.
    */
  case class BoolExp(value: Boolean) extends Expression

  /**
    * Named variable expression.
    */
  case class IdnExp(name: IdnUse) extends Expression

  /**
    * Integer constant expression.
    */
  case class IntExp(value: Int) extends Expression

  /**
    * Conditional expression (if). cond is a Boolean condition. The expression
    * evaluates to the value of left (right) if cond is true (false).
    */
  case class IfExp(cond: Expression, left: Block, right: Block)
      extends Expression

  /**
    * Common superclass of types.
    */
  sealed abstract class Type extends SourceNode

  /**
    * Trait to mark simple types
    */
  trait SimpleType extends Type

  case class UnitType() extends Type with SimpleType {
    override def toString() = "unit"
  }

  /**
    * The basic integer type.
    */
  case class IntType() extends Type with SimpleType {
    override def toString() = "int"
  }

  /**
    * The basic Boolean type.
    */
  case class BoolType() extends Type with SimpleType {
    override def toString() = "bool"
  }

  /**
    * A function type from an argument of type `argType` to a result of type
    * `resType`.
    */
  case class FnType(argTypes: Vector[Type], resType: Type) extends Type {
    override def toString() =
      resType match {
        case _: SimpleType => s"fn(${argTypes.mkString(",")})->${resType}"
        case _             => s"fn(${argTypes.mkString(",")})->(${resType})"
      }
  }

  /**
    * The type of a mutable reference cell containing a value of a specified type
    */
  case class ArrayType(tipe: Type) extends Type {
    override def toString() =
      tipe match {
        case _: SimpleType => s"array ${tipe.toString()}"
        case _             => s"array (${tipe.toString()})"
      }
  }

  /**
    * The type of something whose type we cannot determine.  Compatible
    * with anything.
    */
  case class UnknownType() extends Type with SimpleType {
    override def toString() = "?"
  }

  /**
    * An identifier reference.
    */
  sealed abstract class IdnNode extends SourceNode {
    def idn: String
  }

  /**
    * A defining occurrence of an identifier (i.e., a place where an entity named by
    * the identifier is being introduced).
    */
  case class IdnDef(idn: Identifier) extends IdnNode

  /**
    * An applied occurrence (use) of an identifier (i.e., a place where an entity with
    * this name is being used, but not introduced).
    */
  case class IdnUse(idn: Identifier) extends IdnNode

  /**
    * A representation of identifiers as strings.
    */
  type Identifier = String

}
