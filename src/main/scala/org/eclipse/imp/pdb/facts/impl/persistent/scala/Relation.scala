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
import scala.collection.breakOut
import scala.annotation.tailrec

case class Relation(override val et: Type, override val xs: Set.Coll)
  extends Set(et, xs) with IRelation {
 
  override val t = TypeFactory.getInstance relTypeFromTuple et  
  
  // alias for type casting
  val ts = xs.asInstanceOf[collection.immutable.Set[Tuple]]
  
  override def accept[T](v: IValueVisitor[T]): T = v visitRelation this
  
  def arity = et.getArity
  
  def compose(other: IRelation): IRelation = other match {
    case that: Relation => {
      val relationType = getType compose that.getType
      val tupleType = TypeFactory.getInstance.tupleType(this.et.getFieldType(0), that.et.getFieldType(1)) 

      val otherIndexed = that.xs groupBy { _.asInstanceOf[ITuple].get(0) }

//      val tuples: Set.Coll = for {
//        Tuple(x, y) <- this.ts
//        Tuple(_, z) <- otherIndexed.getOrElse(y, Set.empty)
//      } yield Tuple(tupleType, x, z)

      val tuples: Set.Coll = xs.flatMap { t1 =>
        otherIndexed.getOrElse(t1.asInstanceOf[ITuple].get(1), Set.empty).map { t2 =>
          Tuple(tupleType, t1.asInstanceOf[ITuple].get(0), t2.asInstanceOf[ITuple].get(1)): IValue
        }(breakOut)
      }
	        
      Relation(relationType.getFieldTypes, tuples)
    }
  }
  
  def closure: IRelation = {
    @tailrec def calculate(oldSize: Int, r: Relation): Relation = {
      if (r.size == oldSize) r
      else calculate(r.size, r union (r compose r))
    }

    calculate(0, this)
  }

  def closureStar: IRelation = {
    val resultElementType = getType.closure.getElementType
    val reflex = Relation(resultElementType, (for (x <- carrier.asInstanceOf[Set].xs) yield Tuple(resultElementType, x, x)))
    
    closure union reflex
  }

  def carrier: ISet = {
    val newElementType = getType.carrier.getElementType
    val newElementData = ts flatMap { _.xs }
      
    Set(newElementType, newElementData)
  }

  def getFieldTypes = t.getFieldTypes

  def domain = valuesAtIndex(0)
  
  def range = valuesAtIndex(getType.getArity - 1)

  def valuesAtIndex(i: Int): ISet = Set(getType.getFieldType(i), for (t <- ts) yield t.get(i))  
  
  def select(fields: Int*): ISet = {
    val et = getFieldTypes.select(fields: _*)
    val ys = (for (t <- ts) yield t select(fields: _*))

    SetOrRel(et, ys)
  }  
  
  def selectByFieldNames(fields: String*): ISet = this select ((for (s <- fields) yield (getFieldTypes getFieldIndex s)): _*) 
	
}
