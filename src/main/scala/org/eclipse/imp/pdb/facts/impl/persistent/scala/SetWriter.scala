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

// TODO: fix var t in constructor
class SetWriter(var t: Type) extends ISetWriter {
  
  var xs = collection.immutable.Set[IValue]().empty
  
  def size = xs size
  
  def done: ISet = Set(t, xs)

  def insert(ys: IValue*) { xs = xs ++ ys }       
      
  def insertAll(ys: java.lang.Iterable[_ <: IValue]) { xs = xs ++ ys} 
  
  def delete(x: IValue) {xs = xs - x}

}

class SetWriterWithTypeInference() extends SetWriter(TypeFactory.getInstance voidType) {
    
  override def insert(ys: IValue*) { ys foreach updateType ; super.insert(ys: _*) }       
  
//	// cyclic reference
//  override def insertAll(ys: java.lang.Iterable[_ <: IValue]) { ys foreach updateType ; super.insertAll(ys) }   
  
  private def updateType(x: IValue) = t = t lub x.getType  

  override def done = if (t isTupleType) Relation(t, xs) else Set(t, xs)
  
}