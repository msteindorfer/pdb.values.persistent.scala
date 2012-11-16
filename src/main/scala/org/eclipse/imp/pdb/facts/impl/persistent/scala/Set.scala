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
import org.eclipse.imp.pdb.facts.impl.Value
import org.eclipse.imp.pdb.facts.impl.Writer
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.immutable.Set.empty
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable

// TODO: check variance and invariance
// TODO: check why I don't have to specify setType in super call?
case class Set(t: Type, xs: collection.immutable.Set[IValue])
  extends Value(TypeFactory.getInstance setType t) with ISet {

  private lazy val hash: Int = xs.hashCode;

  protected def lub(e: IValue) = t lub e.getType
  protected def lub(e: ISet) = t lub e.getElementType

  def getElementType = t

  def isEmpty = xs isEmpty

  def size = xs size

  def contains(x: IValue) = xs contains x

  def insert[SetOrRel <: ISet](x: IValue) = Set(this lub x, xs + x).asInstanceOf[SetOrRel]

  def delete[SetOrRel <: ISet](x: IValue) = Set(this lub x, xs - x).asInstanceOf[SetOrRel]

  def union[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => Set(t lub ot, xs | ys).asInstanceOf[SetOrRel]
    case Relation(ot, ys) => Set(t lub ot, xs | ys).asInstanceOf[SetOrRel]
  }

  def intersect[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => Set(t lub ot, xs & ys).asInstanceOf[SetOrRel]
    case Relation(ot, ys) => Set(t lub ot, xs & ys).asInstanceOf[SetOrRel]
  }

  def subtract[SetOrRel <: ISet](other: ISet) = other match {
    case Set(_, ys) => Set(t, xs &~ ys).asInstanceOf[SetOrRel]
    case Relation(_, ys) => Set(t, xs &~ ys).asInstanceOf[SetOrRel]
  }

  def product(other: ISet): IRelation = {
    val calculate = (ot: Type, ys: collection.immutable.Set[IValue]) => {
      val productType = TypeFactory.getInstance tupleType (t, ot)
      Relation(productType, for (x <- xs; y <- ys) yield new Tuple(x, y))
    }
    other match {
      case Set(ot, ys) => calculate(ot, ys)
      case Relation(ot, ys) => calculate(ot, ys)
    }
  }

  def isSubsetOf(other: ISet) = other match {
    case Set(_, ys) => xs subsetOf ys
  }

  def iterator = xs iterator

  def accept[T](v: IValueVisitor[T]): T = v visitSet this

  // TODO: remove duplication
  override def equals(that: Any): Boolean = that match {
    case other: Set => {
      if (this.xs == empty && other.xs == empty) true
      else (this.t comparable other.t) && (this.xs equals other.xs)
    }
    case other: Relation => {
      if (this.xs == empty && other.xs == empty) true
      else (this.t comparable other.t) && (this.xs equals other.xs)
    }    
    case _ => false
  }

  override def hashCode = hash

}