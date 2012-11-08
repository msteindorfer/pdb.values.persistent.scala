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
import org.eclipse.imp.pdb.facts.impl.Value
import org.eclipse.imp.pdb.facts.impl.Writer
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.mutable.ListBuffer
import collection.JavaConversions.iterableAsScalaIterable

// TODO: DRY for method implementations
// TODO: type inference, if type not given
case class ListWriter(eltType: Type) extends IListWriter {
  
  val content: ListBuffer[IValue] = ListBuffer()
  
  def this() = this(TypeFactory.getInstance.voidType)
  
  def insert(elems: IValue*): Unit = elems ++=: content
  
  def insert(elems: Array[IValue], start: Int, length: Int): Unit = (elems slice(start, start + length)) ++=: content 

  def insertAll(elems: java.lang.Iterable[_ <: org.eclipse.imp.pdb.facts.IValue]): Unit = content prependAll elems

  def insertAt(index: Int, elems: IValue*): Unit = content.insertAll(index, elems)

  def insertAt(index: Int, elems: Array[IValue], start: Int, length: Int): Unit = content.insertAll(index, (elems slice(start, start + length)))

  def replaceAt(index: Int, elem: IValue): Unit = content update (index, elem)

  def append(elems: IValue*): Unit = content ++= elems

  def appendAll(elems: java.lang.Iterable[_ <: org.eclipse.imp.pdb.facts.IValue]): Unit = content appendAll elems

  def delete(elem: IValue): Unit = content indexOf (elem) match {
    case -1 => this
    case index => delete(index)
  }
  
  def delete(i: Int): Unit = content.remove(i)

  def done(): IList = List(eltType, collection.immutable.List.empty ++ content)

}