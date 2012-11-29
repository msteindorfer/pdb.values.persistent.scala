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
case class Relation(et: Type, xs: collection.immutable.Set[IValue])
  extends Set(et, xs) with IRelation {

  protected def lub(e: IValue) = et lub e.getType
  protected def lub(e: ISet) = et lub e.getElementType    
  
  override lazy val t = TypeFactory.getInstance relTypeFromTuple et  
   
  
  /*
   * ISet [Overrides]
   */  
  def insert(x: IValue): IRelation = Relation(this lub x, xs + x)  
  
  def delete(x: IValue): IRelation = Relation(this lub x, xs - x)  
  
  def union(other: ISet): IRelation = other match {
    case Set(ot, ys) => Relation(et lub ot, xs | ys)
    case Relation(ot, ys) => Relation(et lub ot, xs | ys)
  }

  def intersect(other: ISet): IRelation = other match {
    case Set(ot, ys) => Relation(et lub ot, xs & ys)
    case Relation(ot, ys) => Relation(et lub ot, xs & ys)
  }

  def subtract(other: ISet): IRelation = other match {
    case Set(_, ys) => Relation(et, xs &~ ys)
    case Relation(_, ys) => Relation(et, xs &~ ys)
  }
  
  override def accept[T](v: IValueVisitor[T]): T = v visitRelation this
   
  
  /*
   * IRelation [Additions]
   */  
  def arity = et getArity

  def compose(that: IRelation): IRelation = that match {
    case other: Relation => {
		val resultType = getType compose other.getType
		// an exception will have been thrown if the relations are not both binary and
		// have a comparable field to compose.
		
		val tuples: collection.immutable.Set[IValue] = for {
		 Tuple(v1) <- this xs;
		 Tuple(v2) <- other xs; 
		 if (v1(1) equals v2(0))
		} yield new Tuple(v1(0), v2(1))

		Relation(resultType getFieldTypes, tuples) 
    }
  }

  def closure: IRelation = {
	getType closure // will throw exception if not binary and reflexive

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

  def getFieldTypes = t getFieldTypes

  def domain: ISet = valuesAtIndex(0)
  
  def range: ISet = valuesAtIndex(getType.getArity - 1)

  def valuesAtIndex(i: Int): ISet = Set(getType.getFieldType(i), for (Tuple(vs) <- xs) yield vs(i))  
  
  def select(fields: Int*) = {
    val t = getFieldTypes.select(fields: _*)
    val ys = (for (x <- xs) yield x.asInstanceOf[ITuple] select(fields: _*))
    if (t isTupleType) Relation(t, ys) else Set(t, ys)
  }
  
  def selectByFieldNames(fields: String*) = this select ((for (s <- fields) yield (getFieldTypes getFieldIndex s)): _*) 
	
}
