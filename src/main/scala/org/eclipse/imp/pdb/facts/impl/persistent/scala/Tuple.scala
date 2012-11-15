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

import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.impl.Value
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor

import collection.immutable.Vector.empty
import collection.JavaConversions.asJavaIterator

// TODO: efficient Tuple from Tuple generation?
// TODO: fix odd invocation of tupleType and bug inside
case class Tuple(xs: Vector[IValue]) 
  extends Value(TypeFactory.getInstance tupleType (xs: _*)) with ITuple {

  def this() = this(empty)
  def this(xs: Array[IValue]) = this(empty ++ xs)
  def this(x: IValue, y: IValue) = this(Vector(x, y))
  
  def arity = xs size
  
  def get(i: Int) = xs(i)

  def get(l: String) = this get (fType getFieldIndex l)

  def set(i: Int, x: IValue) = Tuple(xs updated (i, x))

  def set(l: String, x: IValue) = this set (fType getFieldIndex l, x)
  
  def select(fields: Int*) = Tuple(empty ++ (for (i <- fields) yield xs(i))) 
  
  def selectByFieldNames(fields: String*) = this select ((for (s <- fields) yield (fType getFieldIndex s)): _*) 
  
  def iterator = xs iterator

  def accept[T](v: IValueVisitor[T]): T = v visitTuple this
 
  override def equals(that: Any): Boolean = that match {
    case other: Tuple => {
      (this.fType comparable other.fType) &&
      (this.arity == other.arity) &&
      (0 until arity).forall(i => this.xs(i) equals other.xs(i))
    }
    case _ => false
  }

  private lazy val hash = {  
    val hashFormula = (h: Int, x: IValue) => (h << 1) ^ (h >> 1) ^ x.hashCode
    xs.foldLeft(0)(hashFormula)
  }  
  
  override def hashCode = hash  
 
}