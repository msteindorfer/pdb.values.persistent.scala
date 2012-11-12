package org.eclipse.imp.pdb.facts.impl.persistent.scala

import org.eclipse.imp.pdb.facts.IMapWriter
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.IMap
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.ITuple

import collection.immutable.Map.empty
import collection.JavaConversions.mapAsScalaMap
import collection.JavaConversions.iterableAsScalaIterable

// TODO: type inference, if types not given
case class MapWriter(kt: Type, vt: Type) extends IMapWriter {

  val xs = collection.mutable.Map[IValue, IValue]()
  
  def this() = this(TypeFactory.getInstance.voidType, TypeFactory.getInstance.voidType)

  def put(k: IValue, v: IValue) = xs += (k -> v)
  
  def putAll(other: IMap) = other match {
    case Map(_, _, ys) => xs ++= ys
  }
  
  def putAll(ys: java.util.Map[IValue, IValue]) = xs ++= ys 
  
  def done = Map(kt, vt, empty ++ xs)

  def insert(ys: IValue*): Unit = xs ++= (for (y <- ys; z = y.asInstanceOf[ITuple]) yield z.get(0) -> z.get(1))       
      
  def insertAll(ys: java.lang.Iterable[_ <: IValue]): Unit = ys foreach (this insert _) 

}