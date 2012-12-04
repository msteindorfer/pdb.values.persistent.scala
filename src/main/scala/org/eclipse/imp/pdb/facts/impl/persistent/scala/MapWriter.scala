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

import org.eclipse.imp.pdb.facts.IMapWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.IMap
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.ITuple

import collection.immutable.Map.empty
import collection.JavaConversions.mapAsScalaMap
import collection.JavaConversions.iterableAsScalaIterable

case class MapWriter(kt: Type, vt: Type) extends IMapWriter {

  val xs = collection.mutable.Map[IValue, IValue]()
  
  def put(k: IValue, v: IValue) = xs += (k -> v)
  
  def putAll(other: IMap) = other match {
    case Map(_, _, ys) => xs ++= ys
  }
  
  def putAll(ys: java.util.Map[IValue, IValue]) = xs ++= ys 
  
  def done = Map(kt, vt, empty ++ xs)

  def insert(ys: IValue*): Unit = xs ++= (for (y <- ys; z = y.asInstanceOf[ITuple]) yield z.get(0) -> z.get(1))       
      
  def insertAll(ys: java.lang.Iterable[_ <: IValue]): Unit = ys foreach (this insert _) 

}

class MapWriterWithTypeInference() extends MapWriter(TypeFactory.getInstance voidType, TypeFactory.getInstance voidType) {

  // TODO: move to a common place
  // NOTE: nice example of how to shorten code
  def lub(xs: Traversable[IValue]): Type = {
    xs.foldLeft(TypeFactory.getInstance voidType)((t, x) => t lub x.getType)
  }

  override def done = {
    val zs = empty ++ xs; val lubKeys = this lub zs.keys; val lubValues = this lub zs.values
    Map(lubKeys, lubValues, zs)
  }
  
}