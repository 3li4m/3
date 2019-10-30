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
 * Semantic analysis for the Func332 language.
 */

package lintilla

import org.bitbucket.inkytonik.kiama._

import LintillaTree.SourceTree
import attribution.Attribution

/**
  * Attribute definitions of the Lintilla semantic analyser.
  */
class SemanticAnalysis(val tree: SourceTree) extends Attribution {

  import scala.reflect.ClassTag

  import LintillaTree._
  import SymbolTable._
  import util.Messaging.{
    check, /* checkUse,*/ collectMessages,
    Messages,
    message
  }
  import util.{MultipleEntity, UnknownEntity, Entity}

  // Error reporting.

  /**
    * Collect the semantic error messages for a given tree.
    */
  lazy val errors: Messages =
    collectMessages(tree) {

      // Errors related to defining and applied instances of identifiers.

      case d @ IdnDef(i) if (entity(d) == MultipleEntity()) =>
        message(d, "'" + i + "' is declared more than once in current scope")

      case u @ IdnUse(i) if (entity(u) == UnknownEntity()) =>
        message(u, "'" + i + "' is not declared at this point")

      case e: Expression =>
        check(e) {
          case EqualExp(l, _)
              if (tipe(l) != IntType() &&
                tipe(l) != BoolType() &&
                tipe(l) != UnknownType()) =>
            message(e, "type error, expecting 'int' or 'bool' found " + tipe(l))

          case LetDecl(_, e) if (tipe(e) == UnitType()) =>
            message(
              e,
              "type error, can't bind expression of type 'unit' to a variable"
            )

          case AssignExp(e, _) if (!e.isInstanceOf[DerefExp]) =>
            message(
              e,
              "type error, left hand side of an assignment must refer to an array entry"
            )

          case ForExp(_, _, _, Some(e), _) 
              if (evalIntConst(e) == None || evalIntConst(e) == Some(0)) =>
            message(e, "step expression of a 'for' loop must be a non-zero constant")

          case e: LoopExp if (!inForBody(e)) =>
            message(e, "'loop' must occur in the body of a 'for' loop")
  
          case e: BreakExp if (!inForBody(e)) =>
            message(e, "'break' must occur in the body of a 'for' loop")

        } ++
          message(
            e,
            "type error, expecting '" + exptipe(e) + "' found '" + tipe(e) + "'",
            !iscompatible(exptipe(e), tipe(e))
          )

      case d @ ParamDecl(_, t) if (t == UnitType()) =>
        message(d, "type error, function parameters can't have type 'unit'")

      case tree.parent.pair(t1: UnitType, FnType(_, t2)) if !(t1 eq t2) =>
        message(t1, "type error, function parameters can't have type 'unit'")

    }

  /**
    * Are two types compatible? If they are both function types, then they must
    * both have the same number of parameter types and corresponding parameter
    * and return types must be compatible. Otherwise, if either of them are
    * unknown then we assume an error has already been raised elsewhere so we
    * say they are compatible with anything. Otherwise the two types have to be
    * exactly the same.
    */
  def iscompatible(t1: Type, t2: Type): Boolean =
    t1 match {
      case FnType(in1, out1) =>
        t2 match {
          case FnType(in2, out2) =>
            in1.length == in2.length &&
              (in1 zip in2).forall { case (t1, t2) => iscompatible(t1, t2) } &&
              iscompatible(out1, out2)
          case _ =>
            t2 == UnknownType()
        }
      case ArrayType(t1a) =>
        t2 match {
          case ArrayType(t2a) => iscompatible(t1a, t2a)
          case _ =>
            t2 == UnknownType()
        }
      case _ =>
        (t1 == UnknownType()) || (t2 == UnknownType()) || (t1 == t2)
    }

  /**
    * The environment containing all bindings visible at a particular
    * node in the tree, not including any that are defined at that node.
    */
  val envin: SourceNode => Environment =
    attr {

      // If we are at the program node (root of tree) then the
      // environment in is an empty root environment.
      case _: Program => rootenv()

      // A ForExp starts a new scope that contains its control variable...
      case tree.parent.pair(v: IdnDef, p: ForExp) => enter(envin(p))
      // ... and its body ...
      case tree.parent.pair(bd: Block, ForExp(v, _, _, _, b)) 
        if (bd eq b) => envout(v)
      // .. but its from, to and step expressions are evaluated in the
      // enclosing scope.
      case tree.parent.pair(fr: Expression, p @ ForExp(_, e, _, _, _))
          if (fr eq e) => envin(p)

      // First child of a Block node. Environment in is that of parent plus
      // a new scope. The exceptions to this rule occurs when the block is
      // the body of a function declaration or a 'for' loop, in which case 
      // the new scope should include the parameter declarations or control
      // variable so is handled elsewhere.
      case tree.parent.pair(n, p: Block)
          if (tree.prev(n).isEmpty) =>
            p match {
              case tree.parent(_: FnDecl) => envin(p)
              case tree.parent(ForExp(_, _, _, _, b)) if (b eq p) => envin(p)
              case _ => enter(envin(p)) 
            }

      // A function declaration introduces an extra scope to constrain
      // its parameters and its defining identifier is in scope in its body.
      // This allows us to make directly recursive calls, but it doesn't allow
      // mutually recursive definitions.
      //
      // On the other hand the defining identifiers introduced in
      // 'let' declarations are not in scope in their defining expressions.
      case tree.prev.pair(n, l: IdnDef) =>
        n match {
          case tree.parent(_: FnDecl) => enter(envout(l))
          case tree.parent(p)         => envin(p)
        }

      // Default plumbing, if no other rules apply.

      // ... the environment out of its sibling to its immediate
      // left
      case tree.prev(l) => envout(l)

      // ... and otherwise, the environment in of its parent.
      case tree.parent(p) => envin(p)
    }

  /**
    * The environment containing all bindings visible "after" a
    * particular node in the tree.  I.e., it's the environment at the
    * node plus any new bindings introduced by the node.
    */
  val envout: SourceNode => Environment =
    attr {

      // Block statement. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n: Block => envin(n)

      // For statement. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n: ForExp => envin(n)

      // Function declarations. The environment out is equal to the
      // environment out of its defining identifier child.
      case FnDecl(n, _, _, _) => envout(n)

      // ... ditto 'let' declarations ...
      case LetDecl(n, _) => envout(n)

      // ... ditto parameter declarations.
      case ParamDecl(n, _) => envout(n)

      // The environment out of a identifier definition node is its environment
      // in extended by a binding for that identifier.
      case n @ IdnDef(i) => defineIfNew(envin(n), i, entity(n))

      // Default plumbing, if no other rules apply then a node
      // obtains its environment out from ...

      // ... the environment out of its right-most child ...
      case tree.lastChild(r) => envout(r)

      // ... and if all else fails its environment out is equal to
      // its environment in.
      case n => envin(n)
    }

  /**
    * The program entity referred to by an identifier definition or use.  In
    * the case of a definition it's the thing being defined, so define it to
    * be a reference to a new entity that represents that thing.  If it's
    * already defined, return an entity that indicates a multiple definition.
    * In the case of a use, it's the thing defined elsewhere that is being
    * referred to here, so look it up in the environment, using an unknown
    * entity if the environment does not already contain a binding.
    */
  val entity: IdnNode => Entity =
    attr {

      // At defining instances, if the identifier is already defined
      // in the current scope then attribute with the entity for
      // multiple definitions.
      case n @ IdnDef(i) if (isDefinedInScope(envin(n), i)) =>
        MultipleEntity()

      case tree.parent.pair(n @ IdnDef(i), p) =>
        p match {
          case LetDecl(_, e)   => Variable(e)
          case ParamDecl(_, t) => Argument(t)
          case _: ForExp       => Argument(IntType())
          case p: FnDecl =>
            Function(
              FnType(p.args.map(_.tipe), p.optRet.getOrElse(UnitType())),
              p.body
            )
          case _ => UnknownEntity()
        }

      case n @ IdnUse(i) => lookup(envin(n), i, UnknownEntity())
    }

  /**
    * What is the type of an expression?
    */
  val tipe: Expression => Type =
    attr {

      // Declaration expressions all have 'unit' type...

      case _: LetDecl => UnitType()
      case _: FnDecl  => UnitType()

      // ...so does a 'print' expression...
      case _: PrintExp => UnitType()

      // .. and 'for', 'loop' and 'break' expressions.
      case _: ForExp => UnitType()
      case _: LoopExp => UnitType()
      case _: BreakExp => UnitType() 

      // The type of a block expression is the type of its last expression...

      case tree.lastChild.pair(n: Block, c: Expression) => tipe(c)

      //... or 'unit' if the block is empty...

      case _: Block => UnitType()

      // Application expressions have type equal to the return
      // type of the function being applied.

      case AppExp(l, p) =>
        tipe(l) match {
          case FnType(_, res) => res
          case _              => UnknownType()
        }

      // Typing of arrays, dereferences and assignment.

      case ArrayExp(t) => ArrayType(t)
      case DerefExp(e, _) =>
        tipe(e) match {
          case ArrayType(t) => t
          case _            => UnknownType()
        }
      case _: AssignExp => UnitType()
      case _: AppendExp => UnitType()
      case LengthExp(_) => IntType()

      // The type of an 'if' expression is the type of its 'then' block.
      // We handle the condition that the 'else' block must also have the same
      // type using its 'exptipe' attribute.

      case IfExp(_, l, _) => tipe(l)

      // Boolean expressions

      case AndExp(l, r) => BoolType()
      case OrExp(l, r)  => BoolType()
      case NotExp(e) => BoolType()

      // Relational expressions

      case EqualExp(l, r) => BoolType()
      case LessExp(l, r)  => BoolType()

      // Arithmetic expressions

      case MinusExp(l, r) => IntType()
      case PlusExp(l, r)  => IntType()
      case SlashExp(l, r) => IntType()
      case StarExp(l, r)  => IntType()
      case NegExp(e)      => IntType()

      // Simple expressions

      case BoolExp(_) => BoolType()

      case IdnExp(i) =>
        (entity(i)) match {
          case Variable(e)    => tipe(e)
          case Function(t, _) => t
          case Argument(t)    => t
          case _              => UnknownType()
        }

      case IntExp(_) => IntType()
    }

  /**
    * Assign parameter numbers (from 0) to the actual parameters of
    * each function application. All expression nodes that are not
    * parameters are assigned the parameter number -1.
    */
  val paramno: Expression => Int =
    attr {
      case tree.parent.pair(n, _: AppExp) =>
        n match {
          case tree.prev(p: Expression) => paramno(p) + 1
          case _                        => -1
        }
      case _ => -1
    }

  /**
    * What is the expected type of an expression?  I.e., what type does
    * the context impose on it?  Returns UnknownType if any type will do.
    */
  val exptipe: Expression => Type =
    attr {

      // Applied expression must be a function. We use unknowns for
      // the input and output type of the function because we don't
      // care what they actually are.

      case tree.parent.pair(e, AppExp(l, r)) if e eq l =>
        FnType(r.map(tipe), UnknownType())

      // Each actual parameter is expected to have the same type as the
      // corresponding formal parameter.

      case tree.parent.pair(e: Expression, AppExp(l, _)) =>
        tipe(l) match {
          case FnType(pts, _) if (0 <= paramno(e) && paramno(e) < pts.length) =>
            pts(paramno(e))
          case _ => UnknownType()
        }

      // Expected type of the body block of a function declaration is given by
      // its return type specification.
      case tree.parent.pair(_: Block, p: FnDecl) =>
        p.optRet.getOrElse(UnitType())

      // In 'if' expressions we have:
      // *   The control expression is expected to have type 'bool'.
      // *   The type of the then block can be anything.
      // *   The type of the else block is expected to have the same type as
      //     the then block.

      case tree.parent.pair(e, IfExp(c, _, _)) if e eq c => BoolType()
      case tree.parent.pair(b, IfExp(_, t, e)) if b eq e => tipe(t)

      // All top-level expressions must have type 'unit'.
      case tree.parent(_: Program) => UnitType()

      // All of the children of a block expression, except for the
      // last one, must have type 'unit'. The last one can have any type.

      case tree.parent.pair(e, _: Block) if !tree.next(e).isEmpty => UnitType()

      // Array dereferencing expressions, the expected type of the left hand
      // operand must be an array type, and the expected type of the right hand
      // operand is 'int'.
      case tree.parent.pair(e, DerefExp(l, _)) if (e eq l) =>
        ArrayType(UnknownType())
      case tree.parent.pair(e, DerefExp(_, r)) if (e eq r) => IntType()

      // The parameter to a 'length' expression must be of array type.
      case tree.parent(_: LengthExp) => ArrayType(UnknownType())

      // Assignment, if the LHS is a valid l-value then the RHS must have the same type.
      case tree.parent.pair(e, AssignExp(l: DerefExp, r)) if e eq r => tipe(l)

      // Append, the LHS of an append must have an array type...
      case tree.parent.pair(e, AppendExp(l, r)) if e eq l =>
        ArrayType(UnknownType())
      // .. and the type of the RHS must match the entry type of the array on the LHS.
      case tree.parent.pair(e, AppendExp(l, r)) if e eq r =>
        tipe(l) match {
          case ArrayType(t) => t
          case _            => UnknownType()
        }

      // For expressions...

      // ...have bodies of type 'unit'...
      case tree.parent.pair(e: Expression, ForExp(_, _, _, _, b)) if (e eq b) =>
        UnitType()

      // ...and from, to and step expressions of type 'int'
      case tree.parent.pair(e: Expression, _: ForExp) => IntType()

      // Relational expressions

      // RHS of an equality must have same type as the LHS
      case tree.parent.pair(e, EqualExp(l, r)) if e eq r => tipe(l)

      // The LHS of an equality is handled by the default case below. This says
      // that we don't care what type the LHS of an equality is, even though it
      // has to be 'int' or 'bool'. The reason is that we can only return one
      // type here, but we want to return two. The actual check is made
      // specially in the check function above.

      case tree.parent(_: LessExp) => IntType()

      // Arithmetic expressions

      case tree.parent(_: MinusExp) => IntType()
      case tree.parent(_: PlusExp)  => IntType()
      case tree.parent(_: StarExp)  => IntType()
      case tree.parent(_: SlashExp) => IntType()
      case tree.parent(_: NegExp)   => IntType()

      // Boolean expressions

      case tree.parent(_: AndExp) => BoolType()
      case tree.parent(_: OrExp)  => BoolType()
      case tree.parent(_: NotExp) => BoolType()

      // Other: miscellaneous cases such as:
      // *   the LHS of an equality (see comment above)
      // *   the RHS of an assignment
      // *   the expression in a let declaration
      // *   the then block of an if expression
      // *   the last expression in a block
      // in which we don't care here...

      case _ => UnknownType()
    }

  /**
    * The free variables that occur in an expression. A free variable is
    * one whose meaning is defined outside the expression. E.g., in the
    * expression
    *
    *     fn f(x : int) -> int {
    *          x + y
    *     }
    *
    * `x` is not free since it is bound by the function argument, but
    * `y` is free.
    */
  val freevars: Expression => Set[Identifier] =
    attr {
      case tree.parent.pair(e: Expression, (_: Block | _: Program)) => {
        val from_right: Set[Identifier] = e match {
          case tree.next(n: Expression) => freevars(n)
          case _                        => Set()
        }
        e match {
          case LetDecl(IdnDef(i), _) => (from_right - i) union (freevars_aux(e))
          case FnDecl(IdnDef(i), _, _, b) =>
            (from_right union (freevars_aux(e))) - i
          case _ => from_right union (freevars_aux(e))
        }
      }

      case e: Expression => freevars_aux(e)

    }

  val freevars_aux: Expression => Set[Identifier] =
    attr {
      case LetDecl(_, e) => freevars_aux(e)

      case FnDecl(_, pvs, _, b) =>
        freevars_aux(b) diff (pvs
          .map({ case ParamDecl(IdnDef(i), _) => i })
          .toSet)

      case Block(es) => freevars(es(0))

      case PrintExp(e) => freevars(e)

      case AppExp(f, as) => freevars_aux(f) union as.flatMap(freevars_aux).toSet

      case IfExp(b, t, e) =>
        freevars_aux(b) union freevars_aux(t) union freevars_aux(e)

      case EqualExp(l, r) => freevars_aux(l) union freevars_aux(r)
      case LessExp(l, r)  => freevars_aux(l) union freevars_aux(r)
      case PlusExp(l, r)  => freevars_aux(l) union freevars_aux(r)
      case MinusExp(l, r) => freevars_aux(l) union freevars_aux(r)
      case StarExp(l, r)  => freevars_aux(l) union freevars_aux(r)
      case SlashExp(l, r) => freevars_aux(l) union freevars_aux(r)
      case NegExp(e)      => freevars_aux(e)

      case IdnExp(IdnUse(i)) => Set(i)

      case _ => Set()
    }

  /**
    * Evaluate constant expressions of type 'int'
    */
  val evalIntConst: Expression => Option[Int] =
    attr {
      case IntExp(n) => Some(n)
      case NegExp(e) => for (i <- evalIntConst(e)) yield (-i)
      case PlusExp(l, r) =>
        for (i <- evalIntConst(l); j <- evalIntConst(r)) yield (i + j)
      case MinusExp(l, r) =>
        for (i <- evalIntConst(l); j <- evalIntConst(r)) yield (i - j)
      case StarExp(l, r) =>
        for (i <- evalIntConst(l); j <- evalIntConst(r)) yield (i * j)
      case SlashExp(l, r) =>
        for (i <- evalIntConst(l); j <- evalIntConst(r)) yield (i / j)
      case _ => None
    }

  /**
    * Mark those expressions that occur in the bodies of 'for' statements.
    * We don't propagate these markings into the bodies of 'fn' declarations
    */
  val inForBody: SourceNode => Boolean =
    attr {
      case tree.parent.pair(e: Block, ForExp(_, _, _, _, b)) if (e eq b) => true
      case tree.parent.pair(e: Block, FnDecl(_, _, _, b)) if (e eq b) => false
      case tree.parent(p) => inForBody(p)
      case _ => false
    }

  // A few utility functions.

  /**
    * Test to see if the previous sibling node to a given node
    * has a specified type. Sometimes of use in pattern guards.
    */
  def prevIsOfType[T](n: SourceNode)(implicit tag: ClassTag[T]) =
    n match {
      case tree.prev(_: T) => true
      case _               => false
    }

  /**
    * Test to see if the next sibling node to a given node
    * has a specified type. Sometimes of use in pattern guards.
    */
  def nextIsOfType[T](n: SourceNode)(implicit tag: ClassTag[T]) =
    n match {
      case tree.next(_: T) => true
      case _               => false
    }

  /**
    * Test to see if the parent node of a given node
    * has a specified type. Sometimes of use in pattern guards.
    */
  def parentIsOfType[T](n: SourceNode)(implicit tag: ClassTag[T]) =
    n match {
      case tree.parent(_: T) => true
      case _                 => false
    }

}
