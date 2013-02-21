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

sealed trait Node extends Value with INode {
  
  def name: String
  
  def children: collection.immutable.List[IValue]
  
  def get(i: Int) = children(i)

  def arity = children.length

  def getName = name

  def getChildren = this

  def iterator = children.iterator
  
  def accept[T](v: IValueVisitor[T]): T = v visitNode this

  
  
  def replace(first: Int, second: Int, end: Int, repl: IList) = ???

  
  
  override def hashCode = {
    val hashFormula = (h: Int, x: IValue) => (h << 1) ^ (h >> 1) ^ x.hashCode
    children.foldLeft(name.hashCode)(hashFormula)
  }

  override def equals(that: Any): Boolean = that match {
    case other: Node => {
      (this.t == other.t) &&
        (this.children.length == other.children.length) &&
        (this.name == other.name) &&
        (0 until children.length).forall(i => this.children(i) equals other.children(i))
    }
    case _ => false
  }
  
}

object Node {

  type ChildrenColl = collection.immutable.List[IValue]    
  val emptyChildren = collection.immutable.List.empty[IValue]

  type AnnotationsColl = collection.immutable.Map[String, IValue]
  val emptyAnnotations = collection.immutable.Map.empty[String, IValue]  
  
  def apply(name: String) = SimpleNode(TypeFactory.getInstance.nodeType, name, emptyChildren)

  def apply(name: String, children: ChildrenColl) = SimpleNode(TypeFactory.getInstance.nodeType, name, children)
   
  def apply(name: String, annotations: AnnotationsColl, children: ChildrenColl) = AnnotatedNode(TypeFactory.getInstance.nodeType, name, children, annotations)
  
}

case class SimpleNode(override val t: Type, val name: String, val children: Node.ChildrenColl)
  extends Value with Node {
  
  def set(i: Int, x: IValue) = SimpleNode(t, name, children updated (i, x))
  
  def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedNode(t, name, children, Node.emptyAnnotations ++ newAnnotations)

  def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedNode(t, name, children, Node.emptyAnnotations ++ newAnnotations)

  def setAnnotation(label: String, newValue: IValue) = AnnotatedNode(t, name, children, Node.emptyAnnotations + (label -> newValue))

  def hasAnnotations = false

  def hasAnnotation(label: String) = false  
  
  def getAnnotations = Node.emptyAnnotations
  
  def getAnnotation(label: String) = null

  def removeAnnotations = this  

  def removeAnnotation(key: String) = this
    
}

case class AnnotatedNode(override val t: Type, val name: String, val children: Node.ChildrenColl, val annotations: Node.AnnotationsColl)
  extends Node {
  
  require (annotations.isEmpty == false)  
  
  def set(i: Int, x: IValue) = AnnotatedNode(t, name, children updated (i, x), annotations)
    
  override def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedNode(t, name, children, Node.emptyAnnotations ++ newAnnotations)

  override def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedNode(t, name, children, Node.emptyAnnotations ++ newAnnotations)

  override def setAnnotation(label: String, newValue: IValue) = AnnotatedNode(t, name, children, Node.emptyAnnotations + (label -> newValue))

  def hasAnnotations = true

  def hasAnnotation(label: String) = annotations contains label  
  
  def getAnnotations = annotations
  
  def getAnnotation(label: String) = annotations.getOrElse(label, null)

  def removeAnnotations = SimpleNode(t, name, children)  

  def removeAnnotation(key: String) = (annotations - key) match {
    case newAnnotations => if(newAnnotations.isEmpty) SimpleNode(t, name, children) else AnnotatedNode(t, name, children, newAnnotations)
  }
    
}