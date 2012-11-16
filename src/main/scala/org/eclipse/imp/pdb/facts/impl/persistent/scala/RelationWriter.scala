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

import org.eclipse.imp.pdb.facts.IRelationWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

import collection.immutable.Set.empty
import collection.JavaConversions.mapAsScalaMap
import collection.JavaConversions.iterableAsScalaIterable

class RelationWriter(var t: Type) extends IRelationWriter {
  
  var xs = collection.immutable.Set[IValue]().empty
  
  def size = xs size
  
  def done = Relation(t, xs)

  def insert(ys: IValue*) { xs = xs ++ ys }       
      
  def insertAll(ys: java.lang.Iterable[_ <: IValue]) { xs = xs ++ ys} 
  
  def delete(x: IValue) {xs = xs - x}
}

class RelationWriterWithTypeInference() extends RelationWriter(TypeFactory.getInstance relType (TypeFactory.getInstance voidType)) with IRelationWriter {

  override def insert(ys: IValue*) { ys foreach updateType ; super.insert(ys: _*) }       
      
  override def insertAll(ys: java.lang.Iterable[_ <: IValue]) { ys foreach updateType ; super.insertAll(ys) }   
  
  private def updateType(x: IValue) = t = t lub x.getType    
  
}
  
