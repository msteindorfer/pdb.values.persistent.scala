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

import org.eclipse.imp.pdb.facts.IList
import org.eclipse.imp.pdb.facts.IListWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
import org.eclipse.imp.pdb.facts.impl.Value
import org.eclipse.imp.pdb.facts.impl.Writer
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable

case class List(t: Type, xs: collection.immutable.List[IValue])
  extends Value(TypeFactory.getInstance listType t) with IList {

  private lazy val hash: Int = xs.hashCode;

  private def lub(e: IValue) = t lub e.getType
  private def lub(e: IList) = t lub e.getElementType

  def getElementType = t

  def length = xs length

  def reverse = List(t, xs.reverse)

  def append(x: IValue) = List(this lub x, xs :+ x)

  def insert(x: IValue) = List(this lub x, x :: xs)

  def concat(other: IList) = other match {
    case List(_, ys) => List(this lub other, xs ::: ys)
  }

  def put(i: Int, x: IValue) = List(this lub x, xs updated (i, x))

  def get(i: Int) = xs(i)

  def sublist(i: Int, n: Int) = List(t, xs slice (i, i + n))

  def isEmpty = xs isEmpty

  def contains(e: IValue) = xs contains e

  def delete(x: IValue) = xs indexOf (x) match {
    case i => if (i == -1) this else delete(i)
  }

  def delete(i: Int) = List(t, (xs take i) ::: (xs drop i + 1))

  def iterator = xs iterator

  def accept[T](v: IValueVisitor[T]): T = v visitList this

  override def equals(that: Any): Boolean = that match {
    case other: List => {
      if (this.xs == Nil && other.xs == Nil) true
      else (this.fType comparable other.fType) && (this.xs equals other.xs)
    }
    case _ => false
  }

  override def hashCode = hash

}