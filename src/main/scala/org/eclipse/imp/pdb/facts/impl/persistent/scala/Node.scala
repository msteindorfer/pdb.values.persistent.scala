/*******************************************************************************
 * Copyright (c) 2012 CWI
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

case class Node(override val t: Type, name: String, children: collection.immutable.Vector[IValue], annotations: collection.immutable.Map[String, IValue])
  extends Value with INode {

  def this(name: String) = this(TypeFactory.getInstance nodeType, name, collection.immutable.Vector.empty, collection.immutable.Map.empty)

  def this(name: String, annotations: collection.immutable.Map[String, IValue], children: collection.immutable.Vector[IValue]) = this(TypeFactory.getInstance nodeType, name, children, annotations)

  def this(name: String, children: collection.immutable.Vector[IValue]) = this(TypeFactory.getInstance nodeType, name, children, collection.immutable.Map.empty)

  def get(i: Int) = children(i)

  def set(i: Int, x: IValue) = Node(t, name, children updated (i, x), annotations)

  def arity = children length

  def getName = name

  def getChildren = this

  def iterator = children iterator

  def getAnnotation(label: String) = annotations.getOrElse(label, null)

  def hasAnnotation(label: String) = annotations contains label

  def hasAnnotations = !(annotations isEmpty)

  def getAnnotations = annotations

  def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = Node(t, name, children, collection.immutable.Map.empty ++ newAnnotations)

  def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = Node(t, name, children, annotations ++ newAnnotations)

  def setAnnotation(label: String, newValue: IValue) = Node(t, name, children, annotations + (label -> newValue))

  def removeAnnotation(key: String) = Node(t, name, children, annotations - key)

  def removeAnnotations = Node(t, name, children, collection.immutable.Map.empty)

  def accept[T](v: IValueVisitor[T]): T = v visitNode this

  private lazy val hash = {
    val hashFormula = (h: Int, x: IValue) => (h << 1) ^ (h >> 1) ^ x.hashCode
    children.foldLeft(name.hashCode)(hashFormula)
  }

  override def hashCode = hash

  override def equals(that: Any): Boolean = that match {
    case other: Node => {
      (this.t comparable other.t) &&
        (this.children.length == other.children.length) &&
        (this.name == other.name) &&
        (0 until children.length).forall(i => this.children(i) equals other.children(i))
    }
    case _ => false
  }

}