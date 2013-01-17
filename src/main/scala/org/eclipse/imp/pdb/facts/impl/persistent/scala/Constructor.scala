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

import org.eclipse.imp.pdb.facts.IConstructor
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.`type`.TypeStore

import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable
import collection.JavaConversions.mapAsJavaMap
import collection.JavaConversions.mapAsScalaMap

/*
 * NOTE: Due to inheritance the name receives a copy of <code>t getName</code> whereas it's a computable property.
 */
case class Constructor(override val t: Type, override val children: collection.immutable.List[IValue], override val annotations: collection.immutable.Map[String, IValue])
  extends Node(t, t getName, children, annotations) with IConstructor {
    
  def this(t: Type) = this(t, collection.immutable.List.empty, collection.immutable.Map.empty)
  
  def this(t: Type, children: collection.immutable.List[IValue]) = this(t, children, collection.immutable.Map.empty)

  
  /*
   * INode [Overrides]
   */
  override def set(i: Int, x: IValue) = Constructor(t, children updated (i, x), annotations)

  override def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = Constructor(t, children, collection.immutable.Map.empty ++ newAnnotations)

  override def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = Constructor(t, children, annotations ++ newAnnotations)

  override def setAnnotation(label: String, newValue: IValue) = Constructor(t, children, annotations + (label -> newValue)) 
  
  override def removeAnnotation(key: String) = Constructor(t, children, annotations - key) 

  override def removeAnnotations = Constructor(t, children, collection.immutable.Map.empty)
  
  
  /*
   * ISet [Additions]
   */
  def getConstructorType = t

  /*
   * TODO: improve IConstructor.get(String) lookup time
   * Example usage: (IConstructor) tree.get("prod")
   */
  def get(label: String) = this get (t getFieldIndex label)

  def set(label: String, x: IValue) = this set (t getFieldIndex label, x)

  def has(label: String) = getConstructorType hasField label

  def getChildrenTypes = t getFieldTypes

  def declaresAnnotation(store: TypeStore, label: String) = store.getAnnotationType(getType, label) != null

  
  override def getType = t getAbstractDataType
  
  override def accept[T](v: IValueVisitor[T]): T = v visitConstructor this

  override lazy val hashCode = {
    val hashFormula = (h: Int, x: IValue) => (h << 1) ^ (h >> 1) ^ x.hashCode
    children.foldLeft(name.hashCode)(hashFormula)
  }

  override def equals(that: Any): Boolean = that match {
    case other: Constructor => {
      (this.t comparable other.t) &&
        (this.children.length == other.children.length) &&
        (this.name == other.name) &&
        (0 until children.length).forall(i => this.children(i) equals other.children(i))
    }
    case _ => false
  }  
  
}

//object Constructor {
//  def apply(t: Type, children: collection.immutable.List[IValue], annotations: collection.immutable.Map[String, IValue]): Constructor = new Constructor(t, children, annotations)
//}