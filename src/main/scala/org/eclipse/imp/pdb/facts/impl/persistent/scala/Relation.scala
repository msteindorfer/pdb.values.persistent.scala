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
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.IRelation
import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.immutable.Set.empty
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable

case class Relation(override val et: Type, override val xs: collection.immutable.Set[IValue])
  extends Set(et, xs) with IRelation {
 
  override lazy val t = TypeFactory.getInstance relTypeFromTuple et  
  
  override def accept[T](v: IValueVisitor[T]): T = v visitRelation this
  
  
  /*
   * ISet [Overrides]
   */  
  override def insert[SetOrRel <: ISet](x: IValue) = Relation(this lub x, xs + x).asInstanceOf[SetOrRel]
  
  override def delete[SetOrRel <: ISet](x: IValue) = Relation(this lub x, xs - x).asInstanceOf[SetOrRel]
  
  
  /*
   * IRelation [Additions]
   */  
  def arity = et getArity 
  
  def compose(that: IRelation): IRelation = that match {
    case other: Relation => {
      val resultType = getType compose other.getType
      val otherIndexed = other.xs groupBy { case Tuple(xy) => xy(0) }

      val tuples: collection.immutable.Set[IValue] = for {
        xy <- this.xs.asInstanceOf[collection.immutable.Set[Tuple]];
        yz <- otherIndexed.getOrElse(xy.get(1), empty).asInstanceOf[collection.immutable.Set[Tuple]]
      } yield new Tuple(xy.get(0), yz.get(1))

      Relation(resultType getFieldTypes, tuples)
    }
  }

  def closure: IRelation = {
    getType closure // will throw exception if not binary and reflexive

    def calculate(oldSize: Int, r: Relation): Relation = {
      if (r.size > oldSize) calculate(r.size, r union (r compose r))
      else r
    }

    calculate(0, this)
  }

  def closureStar: IRelation = {
    val resultElementType = getType.closure getElementType
    val reflex = Relation(resultElementType, (for (x <- carrier) yield new Tuple(x, x)).toSet)

    (this closure) union reflex
  }

  def carrier: ISet = {
    val newElementType = getType.carrier getElementType
    val newElementSet = Set(newElementType, (for (Tuple(ys) <- xs) yield ys).flatten)

    newElementSet
  }

  def getFieldTypes = t getFieldTypes

  def domain = valuesAtIndex(0)
  
  def range = valuesAtIndex(getType.getArity - 1)

  def valuesAtIndex(i: Int): ISet = Set(getType.getFieldType(i), for (Tuple(vs) <- xs) yield vs(i))  
  
  def select(fields: Int*) = {
    val t = getFieldTypes.select(fields: _*)
    val ys = (for (x <- xs) yield x.asInstanceOf[ITuple] select(fields: _*))
    if (t isTupleType) Relation(t, ys) else Set(t, ys)
  }
  
  def selectByFieldNames(fields: String*) = this select ((for (s <- fields) yield (getFieldTypes getFieldIndex s)): _*) 
	
}
