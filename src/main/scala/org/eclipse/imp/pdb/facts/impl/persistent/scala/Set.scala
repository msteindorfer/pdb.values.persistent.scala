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

import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.ISetWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.IRelation
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
import org.eclipse.imp.pdb.facts.impl.Writer
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.immutable.Set.empty
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable

case class Set(et: Type, xs: collection.immutable.Set[IValue])
  extends Value with ISet {
  
  protected def lub(e: IValue) = et lub e.getType
  protected def lub(e: ISet) = et lub e.getElementType

  override lazy val t = TypeFactory.getInstance setType et
  
  def getElementType = et

  def isEmpty = xs isEmpty

  def size = xs size

  def contains(x: IValue) = xs contains x

  def insert[SetOrRel <: ISet](x: IValue) = Set(this lub x, xs + x).asInstanceOf[SetOrRel]

  def delete[SetOrRel <: ISet](x: IValue) = Set(this lub x, xs - x).asInstanceOf[SetOrRel]
  
  // TODO: higher order function with operation as parameter
  def union[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => {
      val rt = et lub ot
      val rv = xs | ys

      if (rt isTupleType) Relation(rt, rv) else Set(rt, rv)
    }.asInstanceOf[SetOrRel]
  }

  // TODO: higher order function with operation as parameter
  def intersect[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => {
      val rt = et lub ot
      val rv = xs & ys

      if (rt isTupleType) Relation(rt, rv) else Set(rt, rv)
    }.asInstanceOf[SetOrRel]
  }
  
  // TODO: higher order function with operation as parameter  
  def subtract[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => {
      val rt = et // type is different from union and intersect
      val rv = xs &~ ys

      if (rt isTupleType) Relation(rt, rv) else Set(rt, rv)
    }.asInstanceOf[SetOrRel]
  }

  def product(other: ISet): IRelation = other match {
    case Set(ot, ys) => {
      val productType = TypeFactory.getInstance tupleType (et, ot)
      Relation(productType, for (x <- xs; y <- ys) yield new Tuple(x, y))
    }
  }

  def isSubsetOf(other: ISet) = other match {
    case Set(_, ys) => xs subsetOf ys
  }

  def iterator = xs iterator

  def accept[T](v: IValueVisitor[T]): T = v visitSet this

  override def equals(that: Any): Boolean = that match {
    case other: Set => (this.xs equals other.xs)
    case _ => false
  }

  override lazy val hashCode = xs.hashCode

}