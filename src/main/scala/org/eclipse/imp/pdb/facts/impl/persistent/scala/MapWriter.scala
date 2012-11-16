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

// TODO: fix var kt, vt in constructor
case class MapWriter(var kt: Type, var vt: Type) extends IMapWriter {

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
    
  override def put(k: IValue, v: IValue) = { updateTypes(k, v) ; super.put(k, v) }
  
  override def putAll(ys: IMap) = { ys foreach updateType ; super.putAll(ys) }
  
  override def putAll(ys: java.util.Map[IValue, IValue]) = { ys foreach { case (k, v) => updateTypes(k, v) } ; super.putAll(ys) }
  
  override def insert(ys: IValue*) { ys foreach updateType ; super.insert(ys: _*) }       
      
  override def insertAll(ys: java.lang.Iterable[_ <: IValue]) { ys foreach updateType ; super.insertAll(ys) }   

  private def updateType(kv: IValue) = kv match {
    case Tuple(Vector(k, v)) => updateTypes(k, v)
  }
  
  private def updateTypes(k: IValue, v: IValue) = { kt = kt lub k.getType ;  vt = vt lub v.getType }

}