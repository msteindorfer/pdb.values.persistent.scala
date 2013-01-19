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

import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor

import collection.JavaConversions.asJavaIterator

// TODO: fix odd invocation of tupleType and bug inside
class Tuple(val xs: Tuple.Coll) extends Value with ITuple {
    
  override lazy val t = TypeFactory.getInstance tupleType (xs: _*)
  
  def arity = xs.size
  
  def get(i: Int) = xs(i)

  def get(l: String) = this get (t getFieldIndex l)

  def set(i: Int, x: IValue): ITuple = Tuple(xs updated (i, x))

  def set(l: String, x: IValue): ITuple = this set (t getFieldIndex l, x)
  
  def select(fields: Int*) = { 
    if (t.select(fields: _*).isTupleType)
      Tuple((for (i <- fields) yield xs(i)): _*)
    else
      get(fields(0)) // TODO: ensure that one element is present
  }
  
  def selectByFieldNames(fields: String*) = this select ((for (s <- fields) yield (t getFieldIndex s)): _*) 
  
  def iterator = xs.iterator

  def accept[T](v: IValueVisitor[T]): T = v visitTuple this

  override def equals(other: Any): Boolean = other match {
    case that: Tuple => (this.xs equals that.xs)
    case _ => false
  }

  override lazy val hashCode = xs.hashCode
  
}

object Tuple {  
  type Coll = collection.IndexedSeq[IValue]
  val empty = collection.immutable.Vector.empty[IValue]
  
  def apply(xs: Coll): ITuple = new Tuple(xs)
  def unapply(t: Tuple) = Some(t.xs)
  
  def apply(xs: IValue*): ITuple = new Tuple(empty ++ xs)
}