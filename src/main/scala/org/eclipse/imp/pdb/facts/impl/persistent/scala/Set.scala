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

import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.ISetWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.IRelation
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable

class Set(val et: Type, val xs: Set.Coll)
  extends Value with ISet {
  
  protected def lub(e: IValue) = et lub e.getType
  protected def lub(e: ISet) = et lub e.getElementType

  override val t = TypeFactory.getInstance setType et
  
  def getElementType = et

  def isEmpty = xs.isEmpty

  def size = xs.size

  def contains(x: IValue) = xs contains x

  def insert[SetOrRel <: ISet](x: IValue): SetOrRel = SetOrRel(this lub x, xs + x)

  def delete[SetOrRel <: ISet](x: IValue): SetOrRel = SetOrRel(this lub x, xs - x)
  
  // TODO: higher order function with operation as parameter
  def union[SetOrRel <: ISet](other: ISet): SetOrRel = other match {
    case Set(ot, ys) => {
      val rt = et lub ot
      val rv = xs | ys

      SetOrRel(rt, rv)
    }
  }

  // TODO: higher order function with operation as parameter
  def intersect[SetOrRel <: ISet](other: ISet): SetOrRel = other match {
    case Set(ot, ys) => {
      val rt = et lub ot
      val rv = xs & ys

      SetOrRel(rt, rv)
    }
  }
  
  // TODO: higher order function with operation as parameter  
  def subtract[SetOrRel <: ISet](other: ISet): SetOrRel = other match {
    case Set(ot, ys) => {
      val rt = et // type is different from union and intersect
      val rv = xs &~ ys

      SetOrRel(rt, rv)
    }
  }
  
  def product(other: ISet): IRelation = other match {
    case Set(ot, ys) => {
      val tupleType = TypeFactory.getInstance tupleType (et, ot)
      Relation(tupleType, for (x <- xs; y <- ys) yield Tuple(tupleType, x, y))
    }
  }

  def isSubsetOf(other: ISet) = other match {
    case Set(_, ys) => xs subsetOf ys
  }

  def iterator = xs.iterator

  def accept[T](v: IValueVisitor[T]): T = v visitSet this

  override def equals(other: Any): Boolean = other match {
    case that: Set => (this.xs equals that.xs)
    case _ => false
  }

  override lazy val hashCode = xs.hashCode
  
}

object Set {
  type Coll = collection.immutable.Set[IValue]
  val empty = collection.immutable.Set.empty[IValue]
  
  def apply(et: Type, xs: Coll): ISet = new Set(et, xs)
  def unapply(s: Set) = Some(s.et, s.xs)
}