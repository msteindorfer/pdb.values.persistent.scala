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
import org.eclipse.imp.pdb.facts.IRelation
import org.eclipse.imp.pdb.facts.IListRelation
import org.eclipse.imp.pdb.facts.IValueFactory
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable
import collection.JavaConversions.mapAsJavaMap
import collection.JavaConversions.mapAsScalaMap
import org.eclipse.imp.pdb.facts.impl.fast.FastBaseValueFactory

class ValueFactory extends FastBaseValueFactory {
  
  def tuple = Tuple()

  def tuple(xs: IValue*) = Tuple(xs: _*)

  // TODO: currently the type is ignored and recalculated inside the constructor
  def tuple(t: Type, xs: IValue*) = Tuple(xs: _*)
  
  def node(name: String) = Node(name)

  def node(name: String, children: IValue*) = Node(name, Node.emptyChildren ++ children)

  def node(name: String, children: Array[IValue], keyArgValues: java.util.Map[String,IValue]) = ???  
  
  def node(name: String, annotations: java.util.Map[String, IValue], children: IValue*) = Node(name, Node.emptyChildren ++ children, Node.emptyAnnotations ++ annotations)
  
  def constructor(t: Type) = Constructor(t)

  def constructor(t: Type, children: IValue*) = Constructor(t, Constructor.emptyChildren ++ children)

  def constructor(t: Type, annotations: java.util.Map[String, IValue], children: IValue*): IConstructor = Constructor(t, Constructor.emptyChildren ++ children, Constructor.emptyAnnotations ++ annotations)

  def set(t: Type) = setWriter(t).done

  def set(xs: IValue*) = {
    val writer = setWriter
    writer.insert(xs: _*)
    writer.done
  }

  def setWriter = new SetWriterWithTypeInference()

  def setWriter(t: Type) = new SetWriter(t)

  def list(t: Type) = listWriter(t).done 

  def list(xs: IValue*): IList = {
    val writer = listWriter
    writer.insert(xs: _*)
    writer.done
  }

  def listWriter = new ListWriterWithTypeInference()

  def listWriter(t: Type) = new ListWriter(t)

  def relation(t: Type) = setWriter(t).done.asInstanceOf[IRelation]

  // TODO: add tests, not yet covered
  def relation(xs: IValue*) = set(xs: _*).asInstanceOf[IRelation]
    
  def relationWriter(t: Type) = new RelationWriter(t)

  def relationWriter = new RelationWriterWithTypeInference()

  def map(kt: Type, vt: Type) = mapWriter(kt, vt).done

  def mapWriter = new MapWriterWithTypeInference()

  def mapWriter(kt: Type, vt: Type) = new MapWriter(kt, vt)

  def map(mapType: Type) = mapWriter(mapType).done    
  
  def mapWriter(mapType: Type) = new MapWriter(mapType)

  def listRelation(t: Type) = listRelationWriter(t).done
    
  // TODO: add tests, not yet covered
  def listRelation(xs: IValue*) = list(xs: _*).asInstanceOf[IListRelation]
  
  def listRelationWriter(t: Type) = new ListRelationWriter(t)
  
  def listRelationWriter = new ListRelationWriterWithTypeInference()
  
  override def toString = "VF_SCALA"
  
}