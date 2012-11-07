package org.eclipse.imp.pdb.facts.impl.persistent.scala

import java.util.Iterator
import java.util.{ List => JList }

import org.eclipse.imp.pdb.facts.IList;
import org.eclipse.imp.pdb.facts.IListWriter;
import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException;
import org.eclipse.imp.pdb.facts.exceptions.UnexpectedElementTypeException;
import org.eclipse.imp.pdb.facts.impl.Value;
import org.eclipse.imp.pdb.facts.impl.Writer;
import org.eclipse.imp.pdb.facts.`type`.Type;
import org.eclipse.imp.pdb.facts.`type`.TypeFactory;
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor;
import org.eclipse.imp.pdb.facts.visitors.VisitorException;

import collection.JavaConversions._
import collection.immutable.{ List => SList }

case class List(eltType: Type, javaContent: JList[IValue])
  extends Value(TypeFactory.getInstance().listType(eltType)) with IList {

  private val content: SList[IValue] = javaContent.toList
  private lazy val fHash: Int = content.hashCode();

  private def lub(e: IValue): Type = e.getType.lub(eltType)
  private def lub(e: IList): Type = e.getElementType.lub(eltType)

  def getElementType: Type = eltType

  def length(): Int = content length

  def reverse() = List(eltType, content.reverse)

  def append(e: IValue) = List(this lub e, content :+ e)

  def insert(e: IValue) = List(this lub e, e :: content)

  // optimal implementation?
  def concat(o: IList) = List(this lub o, content ::: (for (e <- o) yield e).toList)

  def put(i: Int, e: IValue) = List(this lub e, content updated (i, e))

  def get(i: Int) = content get i

  def sublist(offset: Int, length: Int) = List(eltType, content slice (offset, offset + length))

  def isEmpty: Boolean = content isEmpty

  def contains(e: IValue): Boolean = content contains e

  def delete(e: IValue) = content indexOf (e) match {
    case -1 => this
    case index => delete(index)
  }

  def delete(i: Int) = {
    val updated = (content take i) ::: (content drop i + 1)
    List(eltType, updated)
  }

  def iterator(): Iterator[IValue] = content.iterator

  def accept[T](v: IValueVisitor[T]): T = v.visitList(this)

  override def equals(o: Any): Boolean = {
    if (getClass() == o.getClass()) {
      val other = o.asInstanceOf[List];

      if (length() == 0 && other.length() == 0) return true;

      return fType.comparable(other.fType) && content.equals(other.content);
    }
    return false;
  }

  override def hashCode(): Int = fHash
  
}