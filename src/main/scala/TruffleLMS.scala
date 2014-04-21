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

import scala.collection.mutable.ArrayBuffer


trait Base {

  // truffle interface
  var runtime: TruffleRuntime = _
  var frameDescriptor: FrameDescriptor = _

  // base types

  trait Typ[T] {
    def slotKind: FrameSlotKind
  }

  type Rep[+T] = Exp[T]

  abstract class BaseNode extends Node with Product {
    def prettyString = productPrefix + "(" + ((0 until productArity) map productElement mkString ",") + ")"
    override def toString = prettyString
  }

  trait Exp[@specialized +T] extends BaseNode {
    def execute(frame: VirtualFrame): T
  }

  case class Sym[@specialized T:Typ](val slot: FrameSlot) extends Exp[T] {
    def execute(frame: VirtualFrame): T = frame.getObject(slot).asInstanceOf[T] // TODO: optimize
  }

  case class Const[@specialized T:Typ](value: T) extends Exp[T] {
    def execute(frame: VirtualFrame): T = value
  }


  trait Def[@specialized T] extends BaseNode {
    def execute(frame: VirtualFrame): T
  }

  trait Stm extends BaseNode {
    def execute(frame: VirtualFrame): Unit
  }

  case class Assign[@specialized T:Typ](slot: FrameSlot, @(Child @field) d: Def[T]) extends Stm {
    def execute(frame: VirtualFrame): Unit = {
      frame.setObject(slot, d.execute(frame)) // TODO: optimize, dispatch on slotkind
    }
  }

  case class Block[@specialized T:Typ](@(Children @field) stms: Array[Stm], res: Exp[T]) extends Node {
    @ExplodeLoop def execute(frame: VirtualFrame): T = {
      var i = 0
      while (i < stms.length) {
        stms(i).execute(frame)
        i += 1
      }
      //stms.foreach(_.execute(frame))
      res.execute(frame)
    }
    override def toString = stms.map(_.toString).mkString("\n")
  }

  // reflect & reify: create statements in evaluation order

  var localDefs: ArrayBuffer[Stm] = null

  var varCount = 0
  def fresh[T:Typ] = {
    varCount += 1
    Sym(frameDescriptor.addFrameSlot(s"x${varCount-1}", implicitly[Typ[T]].slotKind))
  }

  def createDefinition[T:Typ](v: Sym[T], d: Def[T]) = { localDefs += Assign(v.slot,d); v }

  def reflect[T:Typ](d: Def[T]): Exp[T] = createDefinition(fresh,d)

  def reify[T:Typ](d: => Exp[T]): Block[T] = {
    val save = localDefs
    localDefs = new ArrayBuffer
    val res = d
    val stms = localDefs.toArray
    localDefs = save
    Block(stms,res)
  }

  // cross-stage persistance: implicit lifting of constants

  implicit def lift[T:Typ](x: T): Exp[T] = Const(x)


  // access to arguments of Truffle CallTarget

  case class GenericArguments(val values: Array[AnyRef]) extends Arguments

  case class GetArg[T:Typ](index: Int) extends Def[T] {
    def execute(frame: VirtualFrame) = {
      frame.getArguments(classOf[GenericArguments]).values(index).asInstanceOf[T]
    }
  }

  def getArg[T:Typ](index: Int): Exp[T] = reflect(GetArg[T](index))


  // (optional) root node handling

  class LMSRootNode[T](desc: FrameDescriptor, @(Child @field) val block: Block[T]) extends RootNode(null, desc) {
    override def execute(frame: VirtualFrame): AnyRef = block.execute(frame).asInstanceOf[AnyRef]
  }

  def lms[T:Typ,U:Typ](f: Rep[T] => Rep[U]) = new (T=>U) {
    val rootNode = {
      val saveC = varCount
      val saveD = frameDescriptor
      try {
        varCount = 0
        frameDescriptor = new FrameDescriptor();
        new LMSRootNode(frameDescriptor,reify(f(getArg[T](0))));
      } finally {
        //varCount = saveC
        //frameDescriptor = saveD
      }
    }
    val target = runtime.createCallTarget(rootNode)

    override def apply(x: T) = {
      val result = target.call(new GenericArguments(Array(x.asInstanceOf[AnyRef])));
      result.asInstanceOf[U]
    }
  }

  def lms[T1:Typ,T2:Typ,U:Typ](f: (Rep[T1],Rep[T2]) => Rep[U]) = new ((T1,T2)=>U) {
    val rootNode = {
      val saveC = varCount
      val saveD = frameDescriptor
      try {
        varCount = 0
        frameDescriptor = new FrameDescriptor();
        new LMSRootNode(frameDescriptor,reify(f(getArg[T1](0),getArg[T2](1))));
      } finally {
        //varCount = saveC
        //frameDescriptor = saveD
      }
    }
    val target = runtime.createCallTarget(rootNode)

    override def apply(x1: T1, x2: T2) = {
      val result = target.call(new GenericArguments(Array(x1.asInstanceOf[AnyRef],x2.asInstanceOf[AnyRef])));
      result.asInstanceOf[U]
    }
  }


}


trait Primitives extends Base {

  implicit object intTyp extends Typ[Int] {
    def slotKind = FrameSlotKind.Int
  }

  case class IntPlus(@(Child @field) x: Exp[Int], @(Child @field)y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }
  case class IntMinus(@(Child @field) x: Exp[Int], @(Child @field)y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) - y.execute(frame)
    }
  }
  case class IntTimes(@(Child @field) x: Exp[Int], @(Child @field)y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) * y.execute(frame)
    }
  }

  implicit class IntOps(x: Exp[Int]) {
    def +(y: Exp[Int]): Exp[Int] = reflect(IntPlus(x,y))
    def -(y: Exp[Int]): Exp[Int] = reflect(IntMinus(x,y))
    def *(y: Exp[Int]): Exp[Int] = reflect(IntTimes(x,y))
  }

}


trait TruffleLMS extends Base with Primitives
