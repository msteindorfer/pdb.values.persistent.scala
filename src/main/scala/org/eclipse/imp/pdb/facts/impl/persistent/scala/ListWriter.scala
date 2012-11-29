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

import org.eclipse.imp.pdb.facts.IList
import org.eclipse.imp.pdb.facts.IListWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
import org.eclipse.imp.pdb.facts.impl.Writer
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.immutable.List.empty
import collection.mutable.ListBuffer
import collection.JavaConversions.iterableAsScalaIterable

// TODO: fix var t in constructor
class ListWriter(var t: Type) extends IListWriter {

  val xs = ListBuffer[IValue]()

  def insert(ys: IValue*): Unit = ys ++=: xs

  def insert(ys: Array[IValue], i: Int, n: Int) = this insert ((ys slice (i, i + n)): _*)

  def insertAll(ys: java.lang.Iterable[_ <: org.eclipse.imp.pdb.facts.IValue]) = xs prependAll ys

  def insertAt(i: Int, ys: IValue*) = xs insertAll (i, ys)

  def insertAt(i: Int, ys: Array[IValue], j: Int, n: Int) = this insertAt (i, (ys slice (j, j + n)): _*)

  def replaceAt(i: Int, x: IValue) = xs update (i, x)

  def append(ys: IValue*): Unit = xs ++= ys

  def appendAll(ys: java.lang.Iterable[_ <: org.eclipse.imp.pdb.facts.IValue]) = xs appendAll ys

  def delete(x: IValue) = xs indexOf x match {
    case i => if (i == -1) this else delete(i)
  }

  def delete(i: Int) = xs remove i

  def done = List(t, empty ++ xs)

}

class ListWriterWithTypeInference() extends ListWriter(TypeFactory.getInstance voidType) {

  override def insertAt(i: Int, ys: IValue*) = xs insertAll (i, ys)

//  // covered by insertAt(i: Int, ys: IValue*) w.r.t. implementation 
//  def insertAt(i: Int, ys: Array[IValue], j: Int, n: Int)

  override def replaceAt(i: Int, x: IValue) = { updateType(x) ; super.replaceAt(i, x) }

  override def append(ys: IValue*): Unit = { ys foreach updateType ; super.append(ys: _*) }   

  override def appendAll(ys: java.lang.Iterable[_ <: org.eclipse.imp.pdb.facts.IValue]) = { ys foreach updateType ; super.appendAll(ys) } 
  
  override def insert(ys: IValue*) { ys foreach updateType ; super.insert(ys: _*) }       

//  // covered by insert(ys: IValue*) w.r.t. implementation
//  def insert(ys: Array[IValue], i: Int, n: Int)
    
  override def insertAll(ys: java.lang.Iterable[_ <: IValue]) { ys foreach updateType ; super.insertAll(ys) }   

  private def updateType(x: IValue) = t = t lub x.getType

}