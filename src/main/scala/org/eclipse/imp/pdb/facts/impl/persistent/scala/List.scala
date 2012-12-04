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
import org.eclipse.imp.pdb.facts.impl.Writer
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable

case class List(et: Type, xs: collection.immutable.List[IValue])
  extends Value with IList {

  private def lub(e: IValue) = et lub e.getType
  private def lub(e: IList) = et lub e.getElementType

  override lazy val t = TypeFactory.getInstance listType et  
  
  def getElementType = et

  def length = xs length

  def reverse = List(et, xs.reverse)

  def append(x: IValue) = List(this lub x, xs :+ x)

  def insert(x: IValue) = List(this lub x, x :: xs)

  def concat(other: IList) = other match {
    case List(_, ys) => List(this lub other, xs ::: ys)
  }

  def put(i: Int, x: IValue) = List(this lub x, xs updated (i, x))

  def get(i: Int) = xs(i)

  def sublist(i: Int, n: Int) = { 
    if (i < 0 || n < 0 || i + n > length) { throw new IndexOutOfBoundsException() } /* for compatibility with Rascal test suite */
    List(et, xs slice (i, i + n))
  }

  def isEmpty = xs isEmpty

  def contains(e: IValue) = xs contains e

  def delete(x: IValue) = xs indexOf x match {
    case i => if (i == -1) this else delete(i)
  }

  def delete(i: Int) = List(et, (xs take i) ::: (xs drop i + 1))

  def iterator = xs iterator

  def accept[T](v: IValueVisitor[T]): T = v visitList this

  override def equals(that: Any): Boolean = that match {
    case other: List => (this.xs equals other.xs)
    case _ => false
  }

  override lazy val hashCode = xs.hashCode

}