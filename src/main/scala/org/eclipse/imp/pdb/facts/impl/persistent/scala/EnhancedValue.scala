package org.eclipse.imp.pdb.facts.impl.persistent.scala

import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.INode
import org.eclipse.imp.pdb.facts.IList


import collection.JavaConversions.iterableAsScalaIterable
import collection.JavaConversions.mapAsScalaMap

import collection.JavaConverters._

object EnhancedValue {

  type AnnotationsColl = collection.immutable.Map[String, IValue]
  val emptyAnnotations = collection.immutable.Map.empty[String, IValue]

  implicit class WithoutAnnotation[+T <: IValue](val value: T) {
    
    def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = WithAnnotation(value, emptyAnnotations ++ newAnnotations)

    def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = WithAnnotation(value, emptyAnnotations ++ newAnnotations)

    def setAnnotation(label: String, newValue: IValue) = WithAnnotation(value, emptyAnnotations + (label -> newValue))

    def hasAnnotations = false

    def hasAnnotation(label: String) = false

    def getAnnotations = emptyAnnotations

    def getAnnotation(label: String) = null

    def removeAnnotations = this

    def removeAnnotation(key: String) = this

  }

  case class WithAnnotation[+T <: IValue](val value: T, val annotations: AnnotationsColl) {
    
    require(annotations.isEmpty == false)

    def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = WithAnnotation(value, emptyAnnotations ++ newAnnotations)

    def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = WithAnnotation(value, annotations ++ newAnnotations)

    def setAnnotation(label: String, newValue: IValue) = WithAnnotation(value, annotations + (label -> newValue))

    def hasAnnotations = true

    def hasAnnotation(label: String) = annotations contains label

    def getAnnotations = annotations

    def getAnnotation(label: String) = annotations.getOrElse(label, null)

    def removeAnnotations = value // differnent to specialized classes

    def removeAnnotation(key: String) = (annotations - key) match {
      case newAnnotations => if (newAnnotations.isEmpty) WithoutAnnotation(value) else WithAnnotation(value, newAnnotations)
    }

  } 

//  trait Annotated[T <: INode] { // T ~ INode with Annotated[INode]
//
//	val annotations: AnnotationsColl
//	val value: T;
//	
//	def wrap(value: T, annos: AnnotationsColl): T;
//	def unwrap(value: T);
//    
//    def setAnnotations(newAnnotations: java.util.Map[String, IValue]) = wrap(value, emptyAnnotations ++ newAnnotations)
//
//    def joinAnnotations(newAnnotations: java.util.Map[String, IValue]) = wrap(value, annotations ++ newAnnotations)
//
//    def setAnnotation(label: String, newValue: IValue) = wrap(value, annotations + (label -> newValue))
//
//    def hasAnnotations = true
//
//    def hasAnnotation(label: String) = annotations contains label
//
//    def getAnnotations = annotations
//
//    def getAnnotation(label: String) = annotations.getOrElse(label, null)
//
//    def removeAnnotations = unwrap(value)
//
//    def removeAnnotation(key: String) = (annotations - key) match {
//      case newAnnotations => if (newAnnotations.isEmpty) unwrap(value) else wrap(value, newAnnotations)
//    }
//
//  }
   
  trait Annotated[T] extends AnnoPart[T] { // T ~ INode with Annotated[INode]

    self: T =>

  	require (annotations.isEmpty == false)  

    /* Recipe how to wrap and unwrap */
	def wrap(value: T, annos: AnnotationsColl): T;
	def unwrap(value: T): T;
  	
//	def wrap(value: T, annos: AnnotationsColl): T;
//	def unwrap(value: T): T; 	
  	
	def annotations: AnnotationsColl // additional field!
	
    def setAnnotations(newAnnotations: java.util.Map[String, IValue]): T = wrap(this, emptyAnnotations ++ newAnnotations.asScala)

    def joinAnnotations(newAnnotations: java.util.Map[String, IValue]): T = wrap(this, annotations ++ newAnnotations.asScala)

    def setAnnotation(label: String, newValue: IValue): T = wrap(this, annotations + (label -> newValue))

    def hasAnnotations = true

    def hasAnnotation(label: String) = annotations contains label

    def getAnnotations: java.util.Map[String,IValue] = annotations.asJava

    def getAnnotation(label: String) = annotations.getOrElse(label, null)

    def removeAnnotations = unwrap(this)

    def removeAnnotation(key: String) = (annotations - key) match {
      case newAnnotations => if (newAnnotations.isEmpty) unwrap(this) else wrap(this, newAnnotations)
    }

  }
  
  trait Annotatedable[T <: INode] extends AnnoPart[T] { // T ~ INode with Annotated[INode]
    
    self: T =>
    
    /* Recipe how to wrap and unwrap */
	def wrap(value: T, annos: AnnotationsColl): T;
	def unwrap(value: T): T; 		

//	def wrap(value: T, annos: AnnotationsColl): T;
//	def unwrap(value: T): T; 		
	
    def setAnnotations(newAnnotations: java.util.Map[String, IValue]): T = wrap(this, emptyAnnotations ++ newAnnotations.asScala)

    def joinAnnotations(newAnnotations: java.util.Map[String, IValue]): T = wrap(this, emptyAnnotations ++ newAnnotations.asScala)

    def setAnnotation(label: String, newValue: IValue): T = wrap(this, emptyAnnotations + (label -> newValue))

    def hasAnnotations = false

    def hasAnnotation(label: String) = false

    def getAnnotations: java.util.Map[String,IValue] = emptyAnnotations.asJava

    def getAnnotation(label: String) = null

    def removeAnnotations = Annotatedable.this

    def removeAnnotation(key: String) = Annotatedable.this

  }

  trait NodePart {

    def get(i: Int): IValue

    def set(i: Int, newChild: IValue): INode

    def arity: Int

    def getName: String

    def getChildren: java.lang.Iterable[IValue]

    def iterator: java.util.Iterator[IValue]

    def replace(first: Int, second: Int, end: Int, repl: IList): INode

  }

  sealed trait AnnoPart[U] {
    
    def setAnnotations(newAnnotations: java.util.Map[String, IValue]): U

    def joinAnnotations(newAnnotations: java.util.Map[String, IValue]): U

    def setAnnotation(label: String, newValue: IValue): U

    def hasAnnotations: Boolean

    def hasAnnotation(label: String): Boolean

    def getAnnotations: java.util.Map[String, IValue]

    def getAnnotation(label: String): IValue

    def removeAnnotations: U

    def removeAnnotation(key: String): U

  }
  
}

/*
 * From package object.
 */
//  implicit def wrap(value: INode, annos: AnnotationsColl): INode = value match {
//    case node: DifferentNode => new DifferentNode(node.name, node.children) with Annotated[INode] with INode {
//      val annotations = annos
//      override def accept[T](v: IValueVisitor[T]): T = v visitNode this
//    } 
//  }
//
//  implicit def unwrap(value: INode): INode = value match {
//    case node: DifferentNode => new DifferentNode(node.name, node.children) with Annotatedable[INode] with INode {
//      override def accept[T](v: IValueVisitor[T]): T = v visitNode this
//    }
//  }
