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
import org.eclipse.imp.pdb.facts.ITuple
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

// TODO: specialize class for ITuple
// TODO: remove duplicated SetOrRel duplicates [Set replaced by relation in return type]
case class Relation(override val t: Type, override val xs: collection.immutable.Set[IValue])
  extends Set(TypeFactory.getInstance relTypeFromTuple t, xs) with IRelation {

  override def insert[SetOrRel <: ISet](x: IValue) = Relation(this lub x, xs + x).asInstanceOf[SetOrRel]  
  
  override def delete[SetOrRel <: ISet](x: IValue) = Relation(this lub x, xs - x).asInstanceOf[SetOrRel]  
  
  override def union[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => Relation(t lub ot, xs | ys).asInstanceOf[SetOrRel]
  }

  override def intersect[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => Relation(t lub ot, xs & ys).asInstanceOf[SetOrRel]
  }

  override def subtract[SetOrRel <: ISet](other: ISet) = other match {
    case Set(ot, ys) => Relation(t lub ot, xs &~ ys).asInstanceOf[SetOrRel]
  }  
  
  def arity = t getArity

  def compose(that: IRelation): IRelation = that match {
    case other: Relation => {
		val resultType = getType compose other.getType
		// an exception will have been thrown if the relations are not both binary and
		// have a comparable field to compose.
		val w = ValueFactory.getInstance relationWriter (resultType getFieldTypes);

		val tuples: collection.immutable.Set[IValue] = for {
		 Tuple(v1) <- this xs;
		 Tuple(v2) <- other xs; 
		 if (v1(1) equals v2(0))
		} yield new Tuple(v1(0), v2(1))

		Relation(resultType getFieldTypes, tuples) 
    }
  }

  def closure: IRelation = {
    var tmp = this;
    var prevCount = 0;

    while (prevCount != tmp.size) {
      prevCount = tmp.size;
      tmp = tmp union (tmp compose tmp);
    }

    return tmp;
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

  def getFieldTypes = fType getFieldTypes

  def domain: ISet = valuesAtIndex(0)
  
  def range: ISet = valuesAtIndex(getType.getArity - 1)

  def valuesAtIndex(i: Int): ISet = Set(getType.getFieldType(i), for (Tuple(vs) <- xs) yield vs(i))  
  
  def select(fields: Int*) = Set(getFieldTypes.select(fields: _*), (for (x <- xs) yield x.asInstanceOf[ITuple] select(fields: _*))) 
  
  def selectByFieldNames(fields: String*) = this select ((for (s <- fields) yield (getFieldTypes getFieldIndex s)): _*) 
	
  override def accept[T](v: IValueVisitor[T]): T = v visitRelation this

}