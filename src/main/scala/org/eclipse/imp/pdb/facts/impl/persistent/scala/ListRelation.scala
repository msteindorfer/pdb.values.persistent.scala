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

import org.eclipse.imp.pdb.facts.IList
import org.eclipse.imp.pdb.facts.IListRelation
import org.eclipse.imp.pdb.facts.IListWriter
import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.IListRelation
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException
import collection.immutable.List.empty
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable
import scala.annotation.tailrec
import scala.collection.breakOut
import org.eclipse.imp.pdb.facts.ITuple

case class ListRelation(override val et: Type, override val xs: ListColl) extends List(et, xs) with IListRelation {
  
  override lazy val t = TypeFactory.getInstance lrelTypeFromTuple et  
    
  override def accept[T](v: IValueVisitor[T]): T = v visitListRelation this  
  
  /*
   * IListRelation [Additions]
   */  
  def arity = et getArity 
  
  def compose(other: IListRelation): IListRelation = other match {
    case that: ListRelation => {
      val relationType = getType compose that.getType
      val tupleType = TypeFactory.getInstance.tupleType(this.et.getFieldType(0), that.et.getFieldType(1)) 

      val otherIndexed = that.xs groupBy { _.asInstanceOf[ITuple].get(0) }

      val tuples: ListColl = xs.flatMap { t1 =>
        otherIndexed.getOrElse(t1.asInstanceOf[ITuple].get(1), emptyList).map { t2 =>
          Tuple(tupleType, t1.asInstanceOf[ITuple].get(0), t2.asInstanceOf[ITuple].get(1)): IValue
        }(breakOut)
      }

      ListRelation(relationType.getFieldTypes, tuples)
    }
  }

  
  def closure: IListRelation = {
    @tailrec def calculate(oldSize: Int, r: ListRelation): ListRelation = {
      if (r.size > oldSize) calculate(r.size, r concat (r compose r))
      else r 
    }

    calculate(0, this)
  }
  
  def closureStar: IListRelation = {
    val resultElementType = getType.closure getElementType
    val reflex = ListRelation(resultElementType, emptyList ++ (for (x <- carrier) yield Tuple(resultElementType, x, x)))

    closure concat reflex
  }

  def carrier: IList = {
    val newElementType = getType.carrier getElementType
    val newElementSet = List(newElementType, (for (x <- xs) yield x.asInstanceOf[Tuple].xs).flatten)

    newElementSet
  }  
  
  def getFieldTypes = t getFieldTypes

  def domain = valuesAtIndex(0)
  
  def range = valuesAtIndex(getType.getArity - 1)

  def valuesAtIndex(i: Int): IList = List(getType.getFieldType(i), for (x <- xs) yield x.asInstanceOf[ITuple].get(i))  
  
  def select(fields: Int*): IList = {
    val et = getFieldTypes.select(fields: _*)
    val ys = (for (x <- xs) yield x.asInstanceOf[ITuple] select(fields: _*))

    ListOrRel(et, ys)
  }   

  def selectByFieldNames(fields: String*) = this select ((for (s <- fields) yield (getFieldTypes getFieldIndex s)): _*) 
 
  override def replace[ListOrRel <: IList](first: Int, second: Int, end: Int, repl: IList): ListOrRel = ???

  override def equals(that: Any): Boolean = that match {
    case other: List => (this.xs equals other.xs)
    case _ => false
  }

  override lazy val hashCode = xs.hashCode  
  
}