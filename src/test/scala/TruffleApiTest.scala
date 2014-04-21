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

/* 
  Scala adaptation of Truffle API Test Suite

  see:  http://hg.openjdk.java.net/graal/graal/file/483d05bf77a7/graal/com.oracle.truffle.api.test/src/com/oracle/truffle/api/test
*/

class TruffleApiTest extends FunSuite {

/**
 * <h3>Accessing the Truffle Runtime</h3>
 * 
 * <p>
 * The Truffle runtime can be accessed at any point in time globally using the static method
 * {@link Truffle#getRuntime()}. This method is guaranteed to return a non-null Truffle runtime
 * object with an identifying name. A Java Virtual Machine implementation can chose to replace the
 * default implementation of the {@link TruffleRuntime} interface with its own implementation for
 * providing improved performance.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.RootNodeTest}.
 * </p>
 */
  test("TruffleRuntimeTest") {
    val runtime: TruffleRuntime = Truffle.getRuntime();
    assert(runtime != null);
    assert(runtime.getName() != null);
  }

/**
 * <h3>Creating a Root Node</h3>
 * 
 * <p>
 * A Truffle root node is the entry point into a Truffle tree that represents a guest language
 * method. It contains a {@link RootNode#execute(VirtualFrame)} method that can return a
 * {@link java.lang.Object} value as the result of the guest language method invocation. This method
 * must however never be called directly. Instead, the Truffle runtime must be used to create a
 * {@link CallTarget} object from a root node using the
 * {@link TruffleRuntime#createCallTarget(RootNode)} method. This call target object can then be
 * executed using the {@link CallTarget#call()} method or one of its overloads.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.ChildNodeTest}.
 * </p>
 */    

  test("RootNodeTest") {

    class TestRootNode extends RootNode {
      override def execute(frame: VirtualFrame): Integer = 42
    }
    
    val runtime: TruffleRuntime = Truffle.getRuntime();
    val rootNode = new TestRootNode();
    val target: CallTarget = runtime.createCallTarget(rootNode);
    val result = target.call();
    assert(result === 42);
  }

/**
 * <h3>Creating a Child Node</h3>
 * 
 * <p>
 * Child nodes are stored in the class of the parent node in fields that are marked with the
 * {@link Child} annotation. Before such a field is assigned, {@link Node#adoptChild} must be
 * called. This method automatically establishes a link from the child to the parent. The
 * {@link Node#getParent()} method allows access to this field. Every node also provides the ability
 * to iterate over its children using {@link Node#getChildren()}.
 * </p>
 * 
 * <p>
 * A child node field must be declared private and non-final. It may only be assigned in the
 * constructor of the parent node. For changing the structure of the tree at run time, the method
 * {@link Node#replace(Node)} must be used (see {@link ReplaceTest}).
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.ChildrenNodesTest}.
 * </p>
 */

  //TR: out of date comment? adoptChild seems no longer necessary...

  test("ChildNodeTest") {

    class TestRootNodeOrig(left0: TestChildNode, right0: TestChildNode) extends RootNode {
      
      @Child private var left: TestChildNode = left0
      @Child private var right: TestChildNode = left0

      override def execute(frame: VirtualFrame): Integer = left.execute() + right.execute()
    }
    
    class TestRootNode( // alternative, terser declaration
      @(Child @field) left: TestChildNode,
      @(Child @field) right: TestChildNode
    ) extends RootNode {
      override def execute(frame: VirtualFrame): Integer = left.execute() + right.execute()
    }

    class TestChildNode extends Node {
      def execute(): Int = 21
    }

    val runtime = Truffle.getRuntime();
    val leftChild = new TestChildNode();
    val rightChild = new TestChildNode();
    val rootNode = new TestRootNode(leftChild, rightChild);
    val target: CallTarget = runtime.createCallTarget(rootNode);
    assert(rootNode === leftChild.getParent());
    assert(rootNode === rightChild.getParent());
    val iterator = rootNode.getChildren().iterator();
    assert(leftChild === iterator.next());
    assert(rightChild === iterator.next());
    assert(!iterator.hasNext());
    val result = target.call();
    assert(42 == result);

  }

/**
 * <h3>Creating an Array of Children Nodes</h3>
 * 
 * <p>
 * An array of children nodes can be used as a field in a parent node. The field has to be annotated
 * with {@link com.oracle.truffle.api.nodes.Node.Children} and must be declared private and final.
 * Before assigning the field in the parent node constructor, {@link Node#adoptChildren} must be
 * called in order to update the parent pointers in the child nodes. After filling the array with
 * its first values, it must never be changed. It is only possible to call {@link Node#replace} on a
 * child node.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.FinalFieldTest}.
 * </p>
 */
  test("ChildrenNodesTest") {
    class TestRootNode(
      @(Children @field) children: Array[TestChildNode]
    ) extends RootNode {
      @ExplodeLoop override def execute(frame: VirtualFrame): Integer = {
        var sum = 0
        var i = 0
        while (i < children.length) {
          sum += children(i).execute()
          i += 1
        }
        sum
      }
      // alternatives: try 
      //    children.map(_.execute()).sum
      //    children.foreach(um += _.execute())
    }

    class TestChildNode extends Node {
      def execute(): Int = 21
    }

    val runtime = Truffle.getRuntime();
    val leftChild = new TestChildNode();
    val rightChild = new TestChildNode();
    val rootNode = new TestRootNode(Array(leftChild, rightChild));
    val target: CallTarget = runtime.createCallTarget(rootNode);
    assert(rootNode === leftChild.getParent());
    assert(rootNode === rightChild.getParent());
    val iterator = rootNode.getChildren().iterator();
    assert(leftChild === iterator.next());
    assert(rightChild === iterator.next());
    assert(!iterator.hasNext());

    val result = target.call();
    assert(42 === result);
  }

/**
 * <h3>Using Final Fields in Node Classes</h3>
 * 
 * <p>
 * The usage of final fields in node classes is highly encouraged. It is beneficial for performance
 * to declare every field that is not pointing to a child node as final. This gives the Truffle
 * runtime an increased opportunity to optimize this node.
 * </p>
 * 
 * <p>
 * If a node has a value which may change at run time, but will rarely do so, it is recommended to
 * speculate on the field being final. This involves starting executing with a node where this field
 * is final and only if this turns out to be no longer the case, the node is replaced with an
 * alternative implementation of the operation (see {@link ReplaceTest}).
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.ReplaceTest}.
 * </p>
 */
  test("FinalFieldTest") {

    class TestRootNode(
      @(Children @field) children: Array[TestChildNode]
    ) extends RootNode {
      @ExplodeLoop override def execute(frame: VirtualFrame): Integer = {
        var sum = 0; var i = 0
        while (i < children.length) {
          sum += children(i).execute(); i += 1
        }
        sum
      }
    }

    class TestChildNode(value: Int) extends Node {
      def execute(): Int = value
    }
    
    val runtime = Truffle.getRuntime();
    val leftChild = new TestChildNode(20);
    val rightChild = new TestChildNode(22);
    val rootNode = new TestRootNode(Array(leftChild, rightChild));
    val target = runtime.createCallTarget(rootNode);
    val result = target.call();
    assert(42 === result);
  }

/**
 * <h3>Replacing Nodes at Run Time</h3>
 * 
 * <p>
 * The structure of the Truffle tree can be changed at run time by replacing nodes using the
 * {@link Node#replace(Node)} method. This method will automatically change the child pointer in the
 * parent of the node and replace it with a pointer to the new node.
 * </p>
 * 
 * <p>
 * Replacing nodes is a costly operation, so it should not happen too often. The convention is that
 * the implementation of the Truffle nodes should ensure that there are maximal a small (and
 * constant) number of node replacements per Truffle node.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at {@link com.oracle.truffle.api.test.CallTest}.
 * </p>
 */
  test("ReplaceTest") {

    class TestRootNode(
      @(Children @field) children: Array[ValueNode]
    ) extends RootNode {
      @ExplodeLoop override def execute(frame: VirtualFrame): Integer = {
        var sum = 0; var i = 0
        while (i < children.length) {
          sum += children(i).execute(); i += 1
        }
        sum
      }
    }

    abstract class ValueNode extends Node {
      def execute(): Int
    }
    class UnresolvedNode(value: String) extends ValueNode {
      def execute() = {
        val intValue = value.toInt
        val newNode = this.replace(new ResolvedNode(intValue));
        newNode.execute();
      }
    }
    class ResolvedNode(value: Int) extends ValueNode {
      def execute() = value
    }


    val runtime = Truffle.getRuntime();
    val leftChild = new UnresolvedNode("20");
    val rightChild = new UnresolvedNode("22");
    val rootNode = new TestRootNode(Array[ValueNode](leftChild, rightChild));
    val target = runtime.createCallTarget(rootNode);
    val result = target.call();
    assert(42 === result);
    var iterator = rootNode.getChildren().iterator();
    assert(classOf[ResolvedNode] === iterator.next().getClass());
    assert(classOf[ResolvedNode] === iterator.next().getClass());
    assert(!iterator.hasNext)
    iterator = rootNode.getChildren().iterator();
    assert(rootNode === iterator.next().getParent());
    assert(rootNode === iterator.next().getParent());
    assert(!iterator.hasNext)
  }

/**
 * <h3>Calling Another Tree</h3>
 * 
 * <p>
 * A guest language implementation can create multiple call targets using the
 * {@link TruffleRuntime#createCallTarget(RootNode)} method. Those call targets can be passed around
 * as normal Java objects and used for calling guest language methods.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.ArgumentsTest}.
 * </p>
 */
  test("CallTest") {

    class DualCallNode(firstTarget: CallTarget, secondTarget: CallTarget) extends RootNode {
      override def execute(frame: VirtualFrame): Integer =
        firstTarget.call().asInstanceOf[Int] + secondTarget.call().asInstanceOf[Int];
    }

    class ConstantRootNode(value: Int) extends RootNode {
        override def execute(frame: VirtualFrame): Integer = value
    }

    val runtime = Truffle.getRuntime();
    val foo = runtime.createCallTarget(new ConstantRootNode(20));
    val bar = runtime.createCallTarget(new ConstantRootNode(22));
    val main = runtime.createCallTarget(new DualCallNode(foo, bar));
    val result = main.call();
    assert(42 === result);
  }

/**
 * <h3>Passing Arguments</h3>
 * 
 * <p>
 * A guest language can pass its own custom arguments when invoking a Truffle method by creating a
 * subclass of {@link Arguments}. When invoking a call target with
 * {@link CallTarget#call(Arguments)}, the arguments can be passed. A Truffle node can access the
 * arguments passed into the Truffle method by using {@link VirtualFrame#getArguments}.
 * </p>
 * 
 * <p>
 * The arguments class should only contain fields that are declared as final. This allows the
 * Truffle runtime to improve optimizations around guest language method calls. Also, the arguments
 * object must never be stored into a field. It should be created immediately before invoking
 * {@link CallTarget#call(Arguments)} and no longer be accessed afterwards.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at {@link com.oracle.truffle.api.test.FrameTest}
 * .
 * </p>
 */
  test("ArgumentsTest") {

    class TestArguments(val values: Array[Int]) extends Arguments

    class TestRootNode(
      @(Children @field) children: Array[TestArgumentNode]
    ) extends RootNode {
      @ExplodeLoop override def execute(frame: VirtualFrame): Integer = {
        var sum = 0; var i = 0
        while (i < children.length) {
          sum += children(i).execute(frame); i += 1
        }
        sum
      }
    }

    class TestArgumentNode(index: Int) extends Node {
      def execute(frame: VirtualFrame): Int = {
        frame.getArguments(classOf[TestArguments]).values(index);
      }
    }

    val runtime = Truffle.getRuntime();
    val rootNode = new TestRootNode(Array(new TestArgumentNode(0), new TestArgumentNode(1)));
    val target = runtime.createCallTarget(rootNode);

    val result = target.call(new TestArguments(Array(20, 22)));
    assert(42 === result);
  }

/**
 * <h3>Storing Values in Frame Slots</h3>
 * 
 * <p>
 * The frame is the preferred data structure for passing values between nodes. It can in particular
 * be used for storing the values of local variables of the guest language. The
 * {@link FrameDescriptor} represents the current structure of the frame. The method
 * {@link FrameDescriptor#addFrameSlot(Object, FrameSlotKind)} can be used to create predefined
 * frame slots. The setter and getter methods in the {@link Frame} class can be used to access the
 * current value of a particular frame slot. Values can be removed from a frame via the
 * {@link FrameDescriptor#removeFrameSlot(Object)} method.
 * </p>
 * 
 * <p>
 * There are five primitive types for slots available: {@link java.lang.Boolean},
 * {@link java.lang.Integer}, {@link java.lang.Long}, {@link java.lang.Float}, and
 * {@link java.lang.Double} . It is encouraged to use those types whenever possible. Dynamically
 * typed languages can speculate on the type of a value fitting into a primitive (see
 * {@link FrameSlotTypeSpecializationTest}). When a frame slot is of one of those particular
 * primitive types, its value may only be accessed with the respectively typed getter method (
 * {@link Frame#getBoolean}, {@link Frame#getInt}, {@link Frame#getLong}, {@link Frame#getFloat}, or
 * {@link Frame#getDouble}) or setter method ({@link Frame#setBoolean}, {@link Frame#setInt},
 * {@link Frame#setLong}, {@link Frame#setFloat}, or {@link Frame#setDouble}) in the {@link Frame}
 * class.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.FrameSlotTypeSpecializationTest}.
 * </p>
 */
test("FrameTest") {

    class TestRootNode(
      descriptor: FrameDescriptor,
      @(Child @field) left: TestChildNode,
      @(Child @field) right: TestChildNode
    ) extends RootNode(null, descriptor) {
      override def execute(frame: VirtualFrame): Integer = left.execute(frame) + right.execute(frame)
    }

    abstract class TestChildNode extends Node {
      def execute(frame: VirtualFrame): Int
    }

    class AssignLocal(slot: FrameSlot) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        frame.setInt(slot, 42); 0
      }
    }

    class ReadLocal(slot: FrameSlot) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        try {
          frame.getInt(slot);
        } catch { case e: FrameSlotTypeException =>
          throw new IllegalStateException(e);
        }
      }
    }

    val runtime = Truffle.getRuntime();
    val frameDescriptor = new FrameDescriptor();
    val varName = "localVar";
    val slot: FrameSlot = frameDescriptor.addFrameSlot(varName, FrameSlotKind.Int);
    val rootNode = new TestRootNode(frameDescriptor, new AssignLocal(slot), new ReadLocal(slot));
    val target = runtime.createCallTarget(rootNode);
    val result = target.call();
    assert(42 === result);
    frameDescriptor.removeFrameSlot(varName);
    var slotMissing = false;
    try {
        val result = target.call();
    } catch { case iae: IllegalArgumentException =>
        slotMissing = true;
    }
    assert(slotMissing);
  }


/**
 * <h3>Specializing Frame Slot Types</h3>
 * 
 * <p>
 * Dynamically typed languages can speculate on the type of a frame slot and only fall back at run
 * time to a more generic type if necessary. The new type of a frame slot can be set using the
 * {@link FrameSlot#setKind(FrameSlotKind)} method.
 * </p>
 * 
 * <p>
 * The next part of the Truffle API introduction is at
 * {@link com.oracle.truffle.api.test.ReturnTypeSpecializationTest}.
 * </p>
 */
  test("FrameSlotTypeSpecializationTest") {

    class TestRootNode(
      descriptor: FrameDescriptor,
      @(Child @field) var left: TestChildNode,
      @(Child @field) var right: TestChildNode
    ) extends RootNode(null, descriptor) {
      override def execute(frame: VirtualFrame): Object = {
        left.execute(frame)
        right.execute(frame)
      }
    }

    abstract class TestChildNode extends Node {
      def execute(frame: VirtualFrame): Object
    }

    class StringTestChildNode extends TestChildNode {
      def execute(frame: VirtualFrame) = "42"
    }

    class IntAssignLocal(slot: FrameSlot, @(Child @field) var value: TestChildNode) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        value.execute(frame) match {
          case o: Integer =>
            frame.setInt(slot, o);
          case o =>
            frame.setObject(slot, o);
            this.replace(new ObjectAssignLocal(slot, value));            
        }
        null;
      }
    }

    class ObjectAssignLocal(slot: FrameSlot, @(Child @field) var value: TestChildNode) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        val o = value.execute(frame);
        frame.setObject(slot, o);
        null;
      }
    }

    class IntReadLocal(slot: FrameSlot) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        try {
          frame.getInt(slot) : Integer
        } catch { case e: FrameSlotTypeException =>
          this.replace(new ObjectReadLocal(slot)).execute(frame);
        }
      }
    }

    class ObjectReadLocal(slot: FrameSlot) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        try {
          frame.getObject(slot);
        } catch { case e: FrameSlotTypeException =>
          throw new IllegalStateException(e);
        }
      }
    }

    val runtime = Truffle.getRuntime();
    val frameDescriptor = new FrameDescriptor();
    val slot = frameDescriptor.addFrameSlot("localVar", FrameSlotKind.Int);
    val rootNode = new TestRootNode(frameDescriptor, new IntAssignLocal(slot, new StringTestChildNode()), new IntReadLocal(slot));
    val target = runtime.createCallTarget(rootNode);
    assert(FrameSlotKind.Int === slot.getKind());
    val result = target.call();
    assert("42" === result);
    assert(FrameSlotKind.Object === slot.getKind());
  }


/**
 * <h3>Specializing Return Types</h3>
 * 
 * <p>
 * In order to avoid boxing and/or type casts on the return value of a node, the return value the
 * method for executing a node can have a specific type and need not be of type
 * {@link java.lang.Object}. For dynamically typed languages, this return type is something that
 * should be speculated on. When the speculation fails and the child node cannot return the
 * appropriate type of value, it can use an {@link UnexpectedResultException} to still pass the
 * result to the caller. In such a case, the caller must rewrite itself to a more general version in
 * order to avoid future failures of this kind.
 * </p>
 */
  test("ReturnTypeSpecializationTest") {

    class TestRootNode(
      descriptor: FrameDescriptor,
      @(Child @field) var left: TestChildNode,
      @(Child @field) var right: TestChildNode
    ) extends RootNode(null, descriptor) {
      override def execute(frame: VirtualFrame): Object = {
        left.execute(frame)
        right.execute(frame)
      }
    }

    abstract class TestChildNode extends Node {
      def execute(frame: VirtualFrame): Object
      def executeInt(frame: VirtualFrame): Int = {
        val result = execute(frame);
        if (result.isInstanceOf[Integer])
          return result.asInstanceOf[Integer];
        else
          throw new UnexpectedResultException(result);
      }
    }

    class StringTestChildNode extends TestChildNode {
      def execute(frame: VirtualFrame) = "42"
    }

    class IntAssignLocal(slot: FrameSlot, @(Child @field) var value: TestChildNode) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        try {
          val result = value.executeInt(frame);
          frame.setInt(slot, result);
        } catch { case e: UnexpectedResultException =>
          frame.setObject(slot, e.getResult);
          this.replace(new ObjectAssignLocal(slot, value));
        }
        null;
      }
    }

    class ObjectAssignLocal(slot: FrameSlot, @(Child @field) var value: TestChildNode) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        val o = value.execute(frame);
        frame.setObject(slot, o);
        null;
      }
    }

    class IntReadLocal(slot: FrameSlot) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        // can't just call executeInt --> UnexpectedResult
        try {
          frame.getInt(slot) : Integer
        } catch { case e: FrameSlotTypeException =>
          this.replace(new ObjectReadLocal(slot)).execute(frame);
        }
      }
      override def executeInt(frame: VirtualFrame) = {
        try {
          frame.getInt(slot)
        } catch { case e: FrameSlotTypeException =>
          this.replace(new ObjectReadLocal(slot)).executeInt(frame);
        }
      }
    }

    class ObjectReadLocal(slot: FrameSlot) extends TestChildNode {
      def execute(frame: VirtualFrame) = {
        try {
          frame.getObject(slot);
        } catch { case e: FrameSlotTypeException =>
          throw new IllegalStateException(e);
        }
      }
    }

    val runtime = Truffle.getRuntime();
    val frameDescriptor = new FrameDescriptor();
    val slot = frameDescriptor.addFrameSlot("localVar", FrameSlotKind.Int);
    val rootNode = new TestRootNode(frameDescriptor, new IntAssignLocal(slot, new StringTestChildNode()), new IntReadLocal(slot));
    val target = runtime.createCallTarget(rootNode);
    assert(FrameSlotKind.Int === slot.getKind());
    val result = target.call();
    assert("42" === result);
    assert(FrameSlotKind.Object === slot.getKind());
  }

  // TODO: ThreadSafetyTest.java
}

