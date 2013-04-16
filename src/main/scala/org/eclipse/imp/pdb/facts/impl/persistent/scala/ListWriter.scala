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

import org.eclipse.imp.pdb.facts.IList
import org.eclipse.imp.pdb.facts.IListWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException
import org.eclipse.imp.pdb.facts.`type`._
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.mutable.ListBuffer
import collection.JavaConversions.iterableAsScalaIterable

class ListWriter(t: Type) extends IListWriter {
  
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

  def size = xs size
  
  def done: IList = List(t, emptyList ++ xs.result)

}

sealed class ListWriterWithTypeInference() extends ListWriter(TypeFactory.getInstance voidType) {

  override def done: IList = {
    val zs = emptyList ++ xs;
    List(`type` lub zs, zs)
  }
  
}
