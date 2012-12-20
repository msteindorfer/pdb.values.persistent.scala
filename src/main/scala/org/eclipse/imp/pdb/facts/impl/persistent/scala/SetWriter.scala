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

import org.eclipse.imp.pdb.facts.ISetWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

import collection.immutable.Set.empty
import collection.JavaConversions.mapAsScalaMap
import collection.JavaConversions.iterableAsScalaIterable

case class SetWriter(et: Type, var xs: collection.immutable.Set[IValue]) extends ISetWriter {

  def this(t: Type) = this(t, empty) 

  def size = xs size
  
  def done = {
    val elementType = if (xs isEmpty) TypeFactory.getInstance voidType else et
    if (elementType isTupleType) Relation(elementType, xs) else Set(elementType, xs)
  }
  
  def insert(ys: IValue*) { xs = xs ++ ys }       
      
  def insertAll(ys: java.lang.Iterable[_ <: IValue]) { xs = xs ++ ys} 
  
  def delete(x: IValue) {xs = xs - x}

}

case class SetWriterWithTypeInference() extends SetWriter(TypeFactory.getInstance voidType) {
    
  // TODO: move to a common place
  // NOTE: nice example of how to shorten code
  def lub(xs: Traversable[IValue]): Type = {
    xs.foldLeft(TypeFactory.getInstance voidType)((t, x) => t lub x.getType)
  }  
   
  override def done = {
    val zs = empty ++ xs ; val lub = this lub zs 
    if (lub isTupleType) Relation(lub, zs) else Set(lub, zs)
  }
  
}