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
import org.eclipse.imp.pdb.facts.impl.Value
import org.eclipse.imp.pdb.facts.`type`.TypeStore

import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.iterableAsScalaIterable
import collection.JavaConversions.mapAsJavaMap
import collection.JavaConversions.mapAsScalaMap

case class Constructor(t: Type, children: collection.immutable.Vector[IValue], annotations: collection.immutable.Map[String, IValue]) extends Value(t) with IConstructor {
    
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
  
  
  
  
  
  	/**
	 * @return the AbstractDataType of a constructor.
	 */
	override def getType = t getAbstractDataType
	
	/**
	 * @return the specific ConstructorType of this constructor
	 */
	def getConstructorType = t
	
	/**
	 * Get a child from a labeled position in the tree.
	 * @param label the name of the child
	 * @return a value at the position indicated by the label.
	 */
	def get(label: String) = this get (t getFieldIndex label)
	
	/**
	 * Replace a child at a labeled position in the tree. 
	 * @param label    the label of the position
	 * @param newChild the new value of the child
	 * @return a new tree node that is the same as the receiver except for
	 * the fact that at the labeled position the new value has replaced the old value.
	 * All annotations remain equal.
	 * 
	 * @throws FactTypeUseException when this label does not exist for the given tree node, or 
	 *         when the given value has a type that is not a sub-type of the declared type
	 *         of the child with this label.
	 */
	def set(label: String, x: IValue) = this set (t getFieldIndex label, x) 
	
	/**
	 * Find out whether this constructor has a field a given name
	 * @param label name of the field
	 * 
	 * @return true iff this constructor has this field name
	 */
	def has(label: String) = getConstructorType hasField label
		
	/**
	 * @return a tuple type representing the children types of this node/
	 */
	def getChildrenTypes = t getFieldTypes
	
	/**
	 * Check whether a certain annotation label is declared for this type of node.
	 * @param label
	 * @return true iff the given annotation label was declared for this type of node.
	 */
	def declaresAnnotation(store: TypeStore, label: String) = store.getAnnotationType(getType, label) != null	
  
  def accept[T](v: IValueVisitor[T]): T = v visitConstructor this

  
  
  
  
  
  
  private def name: String = t getName
  
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