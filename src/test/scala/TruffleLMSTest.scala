/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._;
import com.oracle.truffle.api.nodes._;
import com.oracle.truffle.api.nodes.Node._;

import scala.annotation.target.field

import org.scalatest._


class TestTruffleLMS extends FunSuite with TruffleLMS {

  /* 
    This test demonstrates the core LMS -> Truffle mechanics:
    
    We create a Truffle RootNode instance that takes an LMS Block[T],
    which is also a Truffle Node.

    The LMS block is obtained via `reify(code)`. The reify operation 
    collects all statements created inside its dynamic scope. The body 
    of `code` uses `lift` to convert Int values to Rep[Int].

    The result AST is equivalent to `x0 = 20 + 22; x0`.
  */

  test("constants") {
    class TestRootNode[T](desc: FrameDescriptor, @(Child @field) var block: Block[T]) extends RootNode(null, desc) {
      override def execute(frame: VirtualFrame): AnyRef = block.execute(frame).asInstanceOf[AnyRef]
    }
    
    def code = {
      lift(20) + lift(22)
    }

    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();

    val rootNode = new TestRootNode(frameDescriptor,reify(code));
    val target: CallTarget = runtime.createCallTarget(rootNode);
    val result = target.call();
    assert(result === 42);

    assert(rootNode.block.toString === 
"""Assign([0,x0,Int],IntPlus(Const(20),Const(22)))""")
  }


  /* 
    Here we do not create a RootNode explicitly but rely on the
    `lms` function, which will create one for us.
    
    Function arguments are also handled internally. We could have 
    dropped the explicit `lift` call, as value 22 would be implicitly 
    converted to Rep[Int].
  */

  test("arguments") {
    val truffelized = lms { x: Rep[Int] =>
      x + lift(22)
    }

    val result = truffelized(20)
    assert(result === 42);

    assert(truffelized.rootNode.block.toString === 
"""Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],IntPlus(Sym([0,x0,Int]),Const(22)))""")

  }


  /* 
    Now we implement the posterchild of staging examples:
    specializing the `power` function to a fixed exponent.

    The Truffle AST contains a function specialized for
    exponent 6.
  */

  test("power") {
    val truffelized = lms { x: Rep[Int] =>
  
      def power(x: Rep[Int], y: Int): Rep[Int] = {
        if (y == 0) 1 else x * power(x,y-1)
      }
      power(x,6)
    }

    val result = truffelized(2)
    assert(result === 64);

    assert(truffelized.rootNode.block.toString === 
"""Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],IntTimes(Sym([0,x0,Int]),Const(1)))
Assign([2,x2,Int],IntTimes(Sym([0,x0,Int]),Sym([1,x1,Int])))
Assign([3,x3,Int],IntTimes(Sym([0,x0,Int]),Sym([2,x2,Int])))
Assign([4,x4,Int],IntTimes(Sym([0,x0,Int]),Sym([3,x3,Int])))
Assign([5,x5,Int],IntTimes(Sym([0,x0,Int]),Sym([4,x4,Int])))
Assign([6,x6,Int],IntTimes(Sym([0,x0,Int]),Sym([5,x5,Int])))""")
  }


  /* 
    The previous example required us to generate a specialized
    AST ahead of time.

    Now we use Truffle's AST rewriting facilities to specialize
    on the fly.

    We introduce a generic Speculate node, that takes a 2-argument
    function as argument and speculates on the second argument
    being stable.

    The power implementation that speculates on a stable exponent
    is then concisely expressed like this:

        lms(speculate(power))
  */

  test("powerSpeculate") {

    case class Speculate(func: (Rep[Int],Int) => Rep[Int], @(Child @field) x: Rep[Int], @(Child @field) y: Rep[Int]) extends Def[Int] {
      var lastY = 0
      @(Child @field) var body = reify(func(x,lastY))

      def execute(frame: VirtualFrame) = {
        val newY = y.execute(frame)
        if (newY != lastY) {
          CompilerDirectives.transferToInterpreterAndInvalidate() // deoptimize!!
          lastY = newY
          body = reify(func(x,lastY))
        }
        body.execute(frame)
      }
    }

    def speculate(f: (Rep[Int],Int)=>Rep[Int]) = (x:Rep[Int],y:Rep[Int]) => reflect(Speculate(f,x,y))

    // 

    def power(x: Rep[Int], y: Int): Rep[Int] = {
      if (y == 0) 1 else x * power(x,y-1)
    }

    val truffelized = lms[Int,Int,Int] { (x,y) => speculate(power)(x,y) }

    val result = truffelized(2,6)
    assert(result === 64);

    // trigger compilation for y = 4

    for (i <- 0 until 1000000)
      truffelized(i,4)

    // should be compiled now -- trigger deoptimization
    // and recompilation for y = 6

    for (i <- 0 until 1000000)
      truffelized(i,6)

    assert(truffelized.rootNode.block.toString === 
"""Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],GetArg(1))
Assign([2,x2,Int],Speculate(<function2>,Sym([0,x0,Int]),Sym([1,x1,Int])))""")  
  }
}

class TestTruffleLMS2 extends FunSuite with TruffleLMS {

  runtime = Truffle.getRuntime();

/*
  loop pattern: while (C) becomes

  val loop = function {
    while {
      val r = C
      if (CompilerDirectives.inInterpreter) return true
      r
    }
    false
  }

  while {
    if (CompilerDirectives.inInterpreter) 
      loop.call(...)
    else
      loop.rootNode.block.execute(...) // inline if parent is compiled, too
  }
*/


  test("loopOSR") {

    def time(s: String)(a: => Unit): Unit = {
      val t0 = System.currentTimeMillis
      a
      val t1 = System.currentTimeMillis
      println(s"$s took ${t1-t0}ms")
    }

    val plain1 = { (x:Int,y:Int) => 
      var w = 0
      var i = 0

      do {
        w = w + (if (0 < x) {x * y / x / y * x * y} else {0})
        i = i + 1
      } while (i < (10*1000000))
      w
    }


    val plain = lms[Int,Int,Int] { (x0,y0) => 

      case class FF() extends Exp[Int] {
        def execute(frame: VirtualFrame):Int = {
          val x = x0.execute(frame)
          val y = y0.execute(frame)
          var w = 0
          var i = 0

          do {
            w = w + (if (0 < x) {x * y / x / y * x * y} else {0})
            i = i + 1
          } while (i < (10*1000000))

          w
        }
      }
      FF()
    }

    val truffelized = lms[Int,Int,Int] { (x,y) => 
      val w = cell(0)
      val i = cell(0)

      val f = fun { () => loopShy {
        w() = w() + (cond (lift(0) < x) {x * y / x / y * x * y} {0})
        i() = i() + 1
        i() < (10*1000000)
      }}

      loop {
        f()
      }
      w()
    }

    println(truffelized.rootNode.block.toString)

    time("A") {
      val result = truffelized(2,6)
      assert(result === 120*1000000);
    }

    time("B") {
      for (i <- 0 until 10)
        truffelized(i,4)
    }

    time("C") {
      for (i <- 0 until 10)
        truffelized(i,6)
    }

    var z = 0
    time("U") {
      val result = plain(2,6)
      assert(result === 120*1000000);
    }

    time("V") {
      for (i <- 0 until 10)
        z += plain(i,4)
    }

    time("W") {
      for (i <- 0 until 10)
        z += plain(i,6)
    }

/*

example timings:

[truffle] opt queued       LMSNestedRootNode...
[truffle] opt start        LMSNestedRootNode...
[truffle] opt done         LMSNestedRootNode...
A took 218ms
[truffle] opt queued       LMSRootNode@318aaf4d 
[truffle] opt start        LMSRootNode@318aaf4d 
[truffle] opt done         LMSRootNode@318aaf4d 
B took 1061ms
C took 984ms
U took 51ms
[truffle] opt queued       LMSRootNode@1e2b0c8d      
[truffle] opt start        LMSRootNode@1e2b0c8d      
[truffle] opt done         LMSRootNode@1e2b0c8d      
V took 489ms
W took 495ms

*/


    println(z)
    assert(truffelized.rootNode.block.toString === 
"""Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],GetArg(1))
Assign([2,x2,Int],Read(Const(0)))
Assign([3,x3,Int],Read(Const(0)))
Assign([19,x19,Boolean],Loop(false,Assign([18,x18,Boolean],LMSNestedCallNode(LMSNestedRootNode@XX <compiled>,LMSNestedRootNode@XX))))
Assign([20,x20,Int],Read(Sym([2,x2,Int])))""")  
  }

}