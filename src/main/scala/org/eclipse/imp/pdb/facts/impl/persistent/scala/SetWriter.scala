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

import org.eclipse.imp.pdb.facts.ISetWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.`type`._
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

import collection.JavaConversions.iterableAsScalaIterable


class SetWriter(et: Type, var xs: Set.Coll) extends ISetWriter {

  def this(t: Type) = this(t, Set.empty) 

  def size = xs.size
  
  def done: ISet = SetOrRel(et, xs)
  
  def insert(ys: IValue*) { xs = xs ++ ys }       
      
  def insertAll(ys: java.lang.Iterable[_ <: IValue]) { xs = xs ++ ys} 
  
  def delete(x: IValue) {xs = xs - x}

}

sealed class SetWriterWithTypeInference() extends SetWriter(TypeFactory.getInstance voidType) {
   
  override def done: ISet = SetOrRel(`type` lub xs, xs)
  
}
