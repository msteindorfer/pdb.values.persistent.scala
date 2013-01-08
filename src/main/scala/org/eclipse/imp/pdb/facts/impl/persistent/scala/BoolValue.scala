package org.eclipse.imp.pdb.facts.impl.persistent.scala

import org.eclipse.imp.pdb.facts.IBool
import org.eclipse.imp.pdb.facts.`type`.TypeFactory
import org.eclipse.imp.pdb.facts.visitors.IValueVisitor

abstract sealed class BoolValue extends Value with IBool {
  
  override def t = TypeFactory.getInstance boolType
  def accept[T](v: IValueVisitor[T]): T = v visitBoolean this
  
  def getStringRepresentation: String = toString
  def equivalent(other: IBool): IBool = if (this eq other) TrueValue else FalseValue

}

case object TrueValue extends BoolValue {

  val getValue: Boolean = true;
  def and(other: IBool): IBool = other
  def or(other: IBool): IBool = this
  def xor(other: IBool): IBool = other.not
  def not: IBool = FalseValue
  def implies(other: IBool): IBool = other

}

case object FalseValue extends BoolValue {

  val getValue: Boolean = false;
  def and(other: IBool): IBool = this
  def or(other: IBool): IBool = other
  def xor(other: IBool): IBool = other
  def not: IBool = TrueValue
  def implies(other: IBool): IBool = TrueValue

}