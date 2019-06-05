// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.silver

import org.scalatest.FunSuite
import viper.silver.ast.utility.rewriter.StrategyBuilder
import viper.silver.frontend._
import viper.silver.verifier.Verifier

class ASTTransformationTests extends FunSuite {
  object frontend extends SilFrontend {
    def configureVerifier(args: Seq[String]): SilFrontendConfig = ???
    def createVerifier(fullCmd: String): Verifier = ???
  }

  test("Rewriting nodes and updating context during parse AST traversal - Example 1") {
    // Transforms this code:
    // method m()
    // {
    //   assert 1 == 1
    // }
    //
    // Into this program:
    // method m()
    // {
    //   assert 3 == 3
    // }

    import viper.silver.parser._

    val binExp1 = PBinExp(PIntLit(1), "==", PIntLit(1))
    val method1 = PMethod(PIdnDef("m"), Seq(), Seq(), Seq(), Seq(), Some(PSeqn(Seq(PAssert(binExp1)))))
    val original = PProgram(Seq(), Seq(), Seq(), Seq(), Seq(), Seq(), Seq(method1), Seq(), Seq())

    val binExp2 = PBinExp(PIntLit(3), "==", PIntLit(3))
    val method2 = PMethod(PIdnDef("m"), Seq(), Seq(), Seq(), Seq(), Some(PSeqn(Seq(PAssert(binExp2)))))
    val target = PProgram(Seq(), Seq(), Seq(), Seq(), Seq(), Seq(), Seq(method2), Seq(), Seq())


    case class Context(increment: Int)

    val transformed = StrategyBuilder.RewriteNodeAndContext[PNode, Context](
      {
        case (PIntLit(i), ctx: Context) =>
          (PIntLit(i + ctx.increment), ctx.copy(ctx.increment + 1)) // Notice that this new context won't
      }, Context(2)).execute[PNode](original)                       // affect its sibling literal node

    assert(transformed === target)
  }

  test("Rewriting nodes and updating context during parse AST traversal - Example 2") {
    // Transform this program:
    // function f(x: Int, y: Int): Int
    // method m()
    // {
    //   assume f(1, 1) == f(1, f(1, f(1, 1)))
    // }
    //
    // Into this one:
    // function f(x: Int, y: Int): Int
    // method m()
    // {
    //   assume f(2, 1) == f(2, f(3, f(4, 1)))
    // }

    import viper.silver.parser._

    val function = PFunction(PIdnDef("f"), Seq(PFormalArgDecl(PIdnDef("x"), TypeHelper.Int), PFormalArgDecl(PIdnDef("y"), TypeHelper.Int)), TypeHelper.Int, Seq(), Seq(), None)
    val assume1 = PAssume(PBinExp(PCall(PIdnUse("f"), Seq(PIntLit(1), PIntLit(1))), "==", PCall(PIdnUse("f"), Seq(PIntLit(1), PCall(PIdnUse("f"), Seq(PIntLit(1), PCall(PIdnUse("f"), Seq(PIntLit(1), PIntLit(1)))))))))
    val method1 = PMethod(PIdnDef("m"), Seq(), Seq(), Seq(), Seq(), Some(PSeqn(Seq(assume1))))
    val original = PProgram(Seq(), Seq(), Seq(), Seq(), Seq(function), Seq(), Seq(method1), Seq(), Seq())

    val assume2 = PAssume(PBinExp(PCall(PIdnUse("f"), Seq(PIntLit(2), PIntLit(1))), "==", PCall(PIdnUse("f"), Seq(PIntLit(2), PCall(PIdnUse("f"), Seq(PIntLit(3), PCall(PIdnUse("f"), Seq(PIntLit(4), PIntLit(1)))))))))
    val method2 = PMethod(PIdnDef("m"), Seq(), Seq(), Seq(), Seq(), Some(PSeqn(Seq(assume2))))
    val target = PProgram(Seq(), Seq(), Seq(), Seq(), Seq(function), Seq(), Seq(method2), Seq(), Seq())

    case class Context(increment: Int)

    val transformed = StrategyBuilder.RewriteNodeAndContext[PNode, Context]({
      case (PCall(fname, PIntLit(i) :: tail, retType), ctx) =>
        (PCall(fname, PIntLit(i + ctx.increment) :: tail, retType), ctx.copy(ctx.increment + 1))
    }, Context(1)).execute[PNode](original)

    assert(transformed === target)
  }

//  test("Rewriting nodes and updating context during parse AST traversal - Example 3(Introducing a new function)") {
//    // Transform this program:
//    // function trialer() : Bool
//    //  requires true
//    //  ensures true
//    // {
//    //   pluginTest(5)
//    // }
//    //
//    // Into this one:
//    // function pluginTest(a: Int): Bool
//    // function trialer() : Bool
//    //  requires true
//    //  ensures true
//    // {
//    //  pluginTest(5)
//    // }
//
//    import viper.silver.parser._
//
//    val pluginfunc: PFunction = PFunction(PIdnDef("pluginTest"), Seq(PFormalArgDecl(PIdnDef("a"), TypeHelper.Int)), TypeHelper.Bool, Seq(), Seq(), None)
//
//    val trialerfunc:PFunction = PFunction(PIdnDef("trialer"), Seq[PFormalArgDecl](), TypeHelper.Bool, Seq(PBoolLit(true)), Seq(PBoolLit(true)),Option(PCall(PIdnUse("pluginTest"), Seq(PIntLit(5)))))
//    val origprog: PProgram = PProgram(Seq(), Seq(), Seq(), Seq(), Seq(trialerfunc), Seq(), Seq(), Seq(), Seq())
//    val finalprog: PProgram = PProgram(Seq(), Seq(), Seq(), Seq(), Seq(trialerfunc, pluginfunc), Seq(), Seq(), Seq(), Seq())
//
//    case class Context(increment: Int)
//
//    val transformed = StrategyBuilder.RewriteNodeAndContext[PNode, Context]({
//      case (e@PProgram(_,_,_,_,func_list,_,_,_,_), ctx) =>
//        (PProgram(e.imports,e.macros,e.domains,e.fields,func_list ++ Seq(pluginfunc),e.predicates,e.methods,e.extensions,e.errors), ctx.copy(ctx.increment + 1), Seq())
//    }, Context(1)).execute[PProgram](origprog)
//
//    assert(transformed === finalprog)
//  }
}
