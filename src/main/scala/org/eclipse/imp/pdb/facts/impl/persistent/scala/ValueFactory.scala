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

import org.eclipse.imp.pdb.facts.impl.BaseValueFactory
import org.eclipse.imp.pdb.facts.IString
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.IConstructor
import org.eclipse.imp.pdb.facts.ISetWriter
import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.IListWriter
import org.eclipse.imp.pdb.facts.IList
import org.eclipse.imp.pdb.facts.IMap
import org.eclipse.imp.pdb.facts.IMapWriter
import org.eclipse.imp.pdb.facts.INode
import org.eclipse.imp.pdb.facts.IValueFactory;
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable
import collection.JavaConversions.mapAsJavaMap
import collection.JavaConversions.mapAsScalaMap

class ValueFactory extends BaseValueFactory {

  // NOTE: nice example of how to shorten code
  private def lub(xs: Seq[IValue]): Type = {
    xs.foldLeft(TypeFactory.getInstance voidType)((t, x) => t lub x.getType)
  }

  def string(cps: Array[Int]) = {
    this string cps.foldLeft(new java.lang.StringBuilder(cps length))((sb, cp) => sb.appendCodePoint(cp)).toString()
  }

  def string(cp: Int) = this string Array(cp)

  def tuple = new Tuple()

  def tuple(xs: IValue*) = new Tuple(collection.immutable.Vector.empty ++ xs)

  def node(name: String) = new Node(name)

  def node(name: String, children: IValue*) = new Node(name, collection.immutable.Vector.empty ++ children)

  def node(name: String, annotations: java.util.Map[String, IValue], children: IValue*) = new Node(name, collection.immutable.Map.empty ++ annotations, collection.immutable.Vector.empty ++ children)

  def constructor(t: Type) = new Constructor(t)

  def constructor(t: Type, children: IValue*) = new Constructor(t, collection.immutable.Vector.empty ++ children)

  def constructor(t: Type, annotations: java.util.Map[String, IValue], children: IValue*): IConstructor = new Constructor(t, collection.immutable.Vector.empty ++ children, collection.immutable.Map.empty ++ annotations)

  def set(t: Type) = if (t isTupleType) Relation(t, collection.immutable.Set.empty) else Set(t, collection.immutable.Set.empty)

  def set(ys: IValue*) = {
    val t = this lub ys
    val xs = collection.immutable.Set.empty ++ ys
    if (t isTupleType) new Relation(t, xs) else new Set(t, xs)
  }

  def setWriter = new SetWriterWithTypeInference()

  def setWriter(t: Type) = if (t isTupleType) new RelationWriter(t) else new SetWriter(t)

  def list(t: Type) = List(t, Nil)

  def list(xs: IValue*) = List(this lub xs, Nil ++ xs)

  def listWriter = new ListWriterWithTypeInference()

  def listWriter(t: Type) = new ListWriter(t)

  def relation(t: Type) = Relation(t, collection.immutable.Set.empty)

  def relation(xs: IValue*) = Relation(this lub xs, collection.immutable.Set.empty ++ xs)

  def relationWriter(t: Type) = new RelationWriter(t)

  def relationWriter = new RelationWriterWithTypeInference()

  def map(kt: Type, vt: Type) = Map(kt, vt, collection.immutable.Map.empty)

  def mapWriter = new MapWriterWithTypeInference()

  def mapWriter(kt: Type, vt: Type) = new MapWriter(kt, vt)

}