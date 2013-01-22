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
import scala.annotation.tailrec

case class Relation(override val et: Type, override val xs: Set.Coll)
  extends Set(et, xs) with IRelation {
 
  override lazy val t = TypeFactory.getInstance relTypeFromTuple et  
  
  override def accept[T](v: IValueVisitor[T]): T = v visitRelation this
  
  def arity = et getArity 
  
  def compose(other: IRelation): IRelation = other match {
    case that: Relation => {
      val resultType = getType compose that.getType
      val otherIndexed = that.xs groupBy { _.asInstanceOf[ITuple].get(0) }

      val tuples: collection.immutable.Set[IValue] = for {
        xy <- this.xs.asInstanceOf[collection.immutable.Set[ITuple]];
        yz <- otherIndexed.getOrElse(xy.get(1), empty).asInstanceOf[collection.immutable.Set[ITuple]]
      } yield Tuple(xy.get(0), yz.get(1))

      Relation(resultType getFieldTypes, tuples)
    }
  }

  
  def closure: IRelation = {
    @tailrec def calculate(oldSize: Int, r: Relation): Relation = {
      if (r.size > oldSize) calculate(r.size, r union (r compose r))
      else r 
    }

    calculate(0, this)
  }

  def closureStar: IRelation = {
    val resultElementType = getType.closure getElementType
    val reflex = Relation(resultElementType, (for (x <- carrier.asInstanceOf[Set].xs) yield Tuple(x, x)).toSet)

    closure union reflex
  }

  def carrier: ISet = {
    val newElementType = getType.carrier getElementType
    val newElementData = (for (tuple <- xs; x <- tuple.asInstanceOf[Tuple].xs) yield x)
    
    Set(newElementType, newElementData)
  }

  def getFieldTypes = t getFieldTypes

  def domain = valuesAtIndex(0)
  
  def range = valuesAtIndex(getType.getArity - 1)

  def valuesAtIndex(i: Int): ISet = Set(getType.getFieldType(i), for (Tuple(vs) <- xs) yield vs(i))  
  
  def select(fields: Int*) = {
    val et = getFieldTypes.select(fields: _*)
    val ys = (for (x <- xs) yield x.asInstanceOf[ITuple] select(fields: _*))

    new SetWriter(et, ys).done
  }  
  
  def selectByFieldNames(fields: String*) = this select ((for (s <- fields) yield (getFieldTypes getFieldIndex s)): _*) 
	
}

//object Relation {
//  def apply(et: Type, xs: collection.immutable.Set[IValue]): Relation = new Relation(et, xs)
//}
