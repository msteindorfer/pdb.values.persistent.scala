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

import org.eclipse.imp.pdb.facts.impl.Value
import org.eclipse.imp.pdb.facts.IMap
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.IMapWriter
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.visitors.VisitorException

import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.asScalaIterator
import collection.JavaConversions.mapAsJavaMap

// TODO: type inference, if type not given
// TODO: containsValue performs check with "equals" whereas common and isSubMap perform check with "isEqual". The same behavior is present in the reference implementation.
case class Map(kt: Type, vt: Type, xs: scala.collection.immutable.Map[IValue, IValue])
  extends Value(TypeFactory.getInstance mapType (kt, vt)) with IMap {

  private lazy val hash: Int = xs.hashCode;

  def isEmpty = xs isEmpty

  def size = xs size

  def put(k: IValue, v: IValue) = Map(this.kt lub k.getType, this.vt lub v.getType, xs + (k -> v))

  def get(k: IValue) = xs getOrElse (k, null)

  def containsKey(k: IValue) = xs contains k

  def containsValue(v: IValue) = xs.values exists (w => w equals v)

  def getKeyType = kt

  def getValueType = vt

  def join(other: IMap): IMap = other match {
    case Map(okt, ovt, ys) =>
      Map(this.kt lub okt, this.vt lub ovt, xs ++ ys)
  }

  def remove(other: IMap): IMap = other match {
    case Map(okt, ovt, ys) =>
      Map(this.kt lub okt, this.vt lub ovt,
        xs -- ys.keySet)
  }

  def compose(other: IMap): IMap = other match {
    case Map(_, ovt, ys) => Map(kt, ovt, for ((k, v) <- xs if ys contains v) yield (k, ys(v)))
  }

  def common(other: IMap) = other match {
    case Map(okt, ovt, ys) =>
      Map(this.kt lub okt, this.vt lub ovt,
        xs filter { case (k, v) => (ys contains k) && (ys(k) isEqual v) })
  }

  def isSubMap(other: IMap) = other match {
    case Map(_, _, ys) => xs forall {
      case (k, v) => ys get k match {
        case None => false
        case Some(w) => v isEqual w
      }
    }
  }

  def iterator = xs.keys iterator

  def valueIterator = xs.values iterator

  @deprecated
  def entryIterator: java.util.Iterator[java.util.Map.Entry[IValue, IValue]] = mapAsJavaMap(xs).entrySet iterator

  def accept[T](v: IValueVisitor[T]): T = v visitMap this

  override def equals(that: Any): Boolean = that match {
    case other: Map => this.xs equals other.xs
    case _ => false
  }

  override def hashCode = hash

}