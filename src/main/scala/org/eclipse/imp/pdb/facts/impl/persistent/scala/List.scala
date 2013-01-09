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
import org.eclipse.imp.pdb.facts.IListRelation
import org.eclipse.imp.pdb.facts.IListWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
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

  // caching length because of O(n) cost 
  lazy val length = xs length

  def reverse[ListOrRel <: IList](): ListOrRel = ListOrRel(et, xs.reverse)

  def append[ListOrRel <: IList](x: IValue): ListOrRel = ListOrRel(this lub x, xs :+ x)

  def insert[ListOrRel <: IList](x: IValue): ListOrRel = ListOrRel(this lub x, x :: xs)

  def concat[ListOrRel <: IList](other: IList): ListOrRel = other match {
    case List(_, ys) => ListOrRel(this lub other, xs ::: ys)
  }

  def put[ListOrRel <: IList](i: Int, x: IValue): ListOrRel = ListOrRel(this lub x, xs updated (i, x))

  def get(i: Int) = xs(i)

  def sublist[ListOrRel <: IList](i: Int, n: Int): ListOrRel = { 
    if (i < 0 || n < 0 || i + n > length) { throw new IndexOutOfBoundsException() } /* for compatibility with Rascal test suite */
    ListOrRel(et, xs slice (i, i + n))
  }

  def isEmpty = xs isEmpty

  def contains(e: IValue) = xs contains e

  def delete[ListOrRel <: IList](x: IValue): ListOrRel = xs indexOf x match {
    case i => if (i == -1) this.asInstanceOf[ListOrRel] else delete(i)
  }

  def delete[ListOrRel <: IList](i: Int): ListOrRel = ListOrRel(et, (xs take i) ::: (xs drop i + 1))

  def intersect[ListOrRel <: IList](other: IList): ListOrRel = other match {
    case List(ot, ys) => {
      val rt = et lub ot
      val rv = for (x <- xs if ys contains x) yield x

      ListOrRel(rt, rv)
    }
  }
  
  def subtract[ListOrRel <: IList](other: IList): ListOrRel = other match {
    case List(ot, ys) => ListOrRel(ot, xs diff ys)
  }

  def product(other: IList): IListRelation = other match {
    case List(ot, ys) => {
      val productType = TypeFactory.getInstance tupleType (et, ot)
      ListOrRel(productType, (for (x <- xs; y <- ys) yield new Tuple(x, y)))
    }
  }

  // TODO: stop if iterator is exhausted
  // NOTE: uses mutable BufferedIterator
  def isSubListOf(other: IList): Boolean = other match {
    case List(ot, ys) => {
      val it = xs.iterator.buffered      
      ys foreach ((y) => if (it.hasNext && it.head == y) it.next); it.isEmpty 
    }
  }
    
  def iterator = xs iterator

  def accept[T](v: IValueVisitor[T]): T = v visitList this

  override def equals(that: Any): Boolean = that match {
    case other: List => (this.xs equals other.xs)
    case _ => false
  }

  override lazy val hashCode = xs.hashCode

}
