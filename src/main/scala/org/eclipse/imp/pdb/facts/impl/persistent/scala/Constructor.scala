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

case class Constructor(override val t: Type, children: collection.immutable.Vector[IValue], annotations: collection.immutable.Map[String, IValue])
  extends Value with IConstructor {
    
  def this(t: Type) = this(t, collection.immutable.Vector.empty, collection.immutable.Map.empty)
  
  def this(t: Type, children: collection.immutable.Vector[IValue]) = this(t, children, collection.immutable.Map.empty)

  def get(i: Int) = children(i)

  def set(i: Int, x: IValue) = Constructor(t, children updated (i, x), annotations)

  def arity = children length

  def getName = t getName

  def getChildren = this

  def iterator = children iterator

  def getAnnotation(label: String) = annotations.getOrElse(label, null)

  def hasAnnotation(label: String) = annotations contains label

  def hasAnnotations = !(annotations isEmpty) 

  def getAnnotations = annotations

  def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = Constructor(t, children, collection.immutable.Map.empty ++ newAnnotations)

  def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = Constructor(t, children, annotations ++ newAnnotations)

  def setAnnotation(label: String, newValue: IValue) = Constructor(t, children, annotations + (label -> newValue)) 
  
  def removeAnnotation(key: String) = Constructor(t, children, annotations - key) 

  def removeAnnotations = Constructor(t, children, collection.immutable.Map.empty)
  
  
  
  
  
	override def getType = t getAbstractDataType

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
  
  def accept[T](v: IValueVisitor[T]): T = v visitConstructor this

  
  
  
  
  
  
  private val name: String = t getName
  
  private lazy val hash = {  
    val hashFormula = (h: Int, x: IValue) => (h << 1) ^ (h >> 1) ^ x.hashCode
    children.foldLeft(name.hashCode)(hashFormula)
  }  
  
  override def hashCode = hash    

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