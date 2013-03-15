/*******************************************************************************
 * Copyright (c) 2012-2013 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 *******************************************************************************/
package org.eclipse.imp.pdb.facts.impl.persistent.scala

import org.eclipse.imp.pdb.facts.INode
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable
import collection.JavaConversions.mapAsJavaMap
import collection.JavaConversions.mapAsScalaMap
import org.eclipse.imp.pdb.facts.IList
import EnhancedValue._

class DifferentNode(val name: String, val children: DifferentNode.ChildrenColl)
  extends Value with NodePart with java.lang.Iterable[IValue] {
    
  override val t = TypeFactory.getInstance.nodeType
    
  def set(i: Int, x: IValue): INode = DifferentNode(name, children updated (i, x))  
  
  def get(i: Int) = children(i)

  def arity = children.length

  def getName = name

  def getChildren: java.lang.Iterable[IValue] = this

  def iterator = children.iterator
    
  def accept[T](v: IValueVisitor[T]): T = this match {
    case current: INode => v visitNode current 
  }
  
  def replace(first: Int, second: Int, end: Int, repl: IList) = ???
  
  def wrap(value: INode, annos: AnnotationsColl): INode = value match {
    case node: DifferentNode => new DifferentNode(node.name, node.children) with Annotated[INode] with INode {
      val annotations = annos
    } 
  }

  def unwrap(value: INode): INode = value match {
    case node: DifferentNode => new DifferentNode(node.name, node.children) with Annotatedable[INode] with INode
  }
  
}

object DifferentNode {

  type ChildrenColl = collection.immutable.List[IValue]    
  val emptyChildren = collection.immutable.List.empty[IValue]

  def apply(name: String): INode = apply(name, emptyChildren)

  def apply(name: String, children: ChildrenColl): INode = new DifferentNode(name, children) with Annotatedable[INode] with INode
  
//  def apply(name: String, children: ChildrenColl): INode = new DifferentNode(name, children) with Annotatedable[INode] with INode {
//    override def wrap(value: INode, annos: AnnotationsColl) = value match {
//      case node: DifferentNode => new DifferentNode(node.name, node.children) with Annotatedable[INode] with INode
//    }
//  }
   
//  def apply(name: String, children: ChildrenColl, annotations: AnnotationsColl): INode = this.apply(name, children).setAnnotations(annotations)
  
}

//object WrappedNode {
//  
//  type ChildrenColl = collection.immutable.List[IValue]    
//  val emptyChildren = collection.immutable.List.empty[IValue]  
//  
//  def apply(name: String): INode = apply(name, emptyChildren)
//
//  def apply(name: String, children: ChildrenColl): INode = new DifferentNode(name, children) with Annotatedable[INode] with INode {
//    def wrap = WrappedNode.wrap
//  }
//  
//  def wrap(value: INode, annos: AnnotationsColl): INode = value match {
//    case node: DifferentNode => new DifferentNode(node.name, node.children) with Annotated[INode] with INode {
//      val annotations = annos
//    } 
//  }
//
//  def unwrap(value: INode): INode = value match {
//    case node: DifferentNode => new DifferentNode(node.name, node.children) with Annotatedable[INode] with INode
//  }  
//    
//}