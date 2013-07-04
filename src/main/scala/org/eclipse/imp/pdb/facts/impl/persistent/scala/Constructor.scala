/*******************************************************************************
 * Copyright (c) 2012-2013 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *    * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI
 ******************************************************************************/
package org.eclipse.imp.pdb.facts.impl.persistent.scala

import org.eclipse.imp.pdb.facts.IConstructor
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.`type`.TypeStore
import collection.JavaConversions.asJavaIterator
import collection.JavaConversions.mapAsJavaMap
import collection.JavaConversions.mapAsScalaMap
import org.eclipse.imp.pdb.facts.IList

sealed trait Constructor extends Value with IConstructor {

	def name: String

	def children: Constructor.ChildrenColl

	def get(i: Int) = children(i)

	def arity = children.length

	def getName = name

	def getChildren = this

	def iterator = children.iterator

	def accept[T, E <: Throwable](v: IValueVisitor[T, E]): T = v visitConstructor this

	/*
	 * TODO: improve IConstructor.get(String) lookup time
	 * Example usage: (IConstructor) tree.get("prod")
	 */
	def get(label: String) = this get (t getFieldIndex label)

	def set(label: String, x: IValue) = this set(t getFieldIndex label, x)

	def has(label: String) = getConstructorType hasField label

	def getChildrenTypes = t.getFieldTypes

	def declaresAnnotation(store: TypeStore, label: String) = store.getAnnotationType(getType, label) != null


	def replace(first: Int, second: Int, end: Int, repl: IList) = ???

	def hasKeywordArguments: Boolean = ???

	def getKeywordArgumentNames: Array[String] = ???

	def getKeywordIndex(name: String): Int = ???

	def getKeywordArgumentValue(name: String): IValue = ???

	def positionalArity: Int = ???

}

object Constructor {

	type ChildrenColl = collection.immutable.Vector[IValue]
	val emptyChildren = collection.immutable.Vector.empty[IValue]

	//  type ChildrenColl = scala.Array[IValue]
	//  val emptyChildren = scala.Array.empty[IValue]

	type AnnotationsColl = collection.immutable.Map[String, IValue]
	val emptyAnnotations = collection.immutable.Map.empty[String, IValue]

	def apply(ct: Type) = SimpleConstructor(ct, emptyChildren)

	def apply(ct: Type, children: ChildrenColl) = SimpleConstructor(ct, children)

	def apply(ct: Type, children: ChildrenColl, annotations: AnnotationsColl) = AnnotatedConstructor(ct, children, annotations)

}

case class SimpleConstructor(val ct: Type, val children: Constructor.ChildrenColl)
	extends Constructor {

	def getConstructorType = ct

	def getUninstantiatedConstructorType = ???

	override def t = ct.getAbstractDataType

	def set(i: Int, x: IValue) = SimpleConstructor(ct, children updated(i, x))

	def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedConstructor(ct, children, Constructor.emptyAnnotations ++ newAnnotations)

	def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedConstructor(ct, children, Constructor.emptyAnnotations ++ newAnnotations)

	def setAnnotation(label: String, newValue: IValue) = AnnotatedConstructor(ct, children, Constructor.emptyAnnotations + (label -> newValue))

	def hasAnnotations = false

	def hasAnnotation(label: String) = false

	def getAnnotations = Constructor.emptyAnnotations

	def getAnnotation(label: String) = null

	def removeAnnotation(key: String) = this

	def removeAnnotations = this

	def name = ct.getName

}

case class AnnotatedConstructor(val ct: Type, val children: Constructor.ChildrenColl, val annotations: Constructor.AnnotationsColl)
	extends Constructor {

	require(annotations.isEmpty == false)

	def getConstructorType = ct

	def getUninstantiatedConstructorType = ???

	override def t = ct.getAbstractDataType

	def set(i: Int, x: IValue) = AnnotatedConstructor(ct, children updated(i, x), annotations)

	override def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedConstructor(ct, children, Constructor.emptyAnnotations ++ newAnnotations)

	override def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = AnnotatedConstructor(ct, children, Constructor.emptyAnnotations ++ newAnnotations)

	override def setAnnotation(label: String, newValue: IValue) = AnnotatedConstructor(ct, children, Constructor.emptyAnnotations + (label -> newValue))

	def hasAnnotations = true

	def hasAnnotation(label: String) = annotations contains label

	def getAnnotations = annotations

	def getAnnotation(label: String) = annotations.getOrElse(label, null)

	def removeAnnotations = SimpleConstructor(ct, children)

	def removeAnnotation(key: String) = (annotations - key) match {
		case newAnnotations => if (newAnnotations.isEmpty) SimpleConstructor(ct, children) else AnnotatedConstructor(ct, children, newAnnotations)
	}

	def name = ct.getName

}
