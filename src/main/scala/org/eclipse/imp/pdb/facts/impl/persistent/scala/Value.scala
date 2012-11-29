package org.eclipse.imp.pdb.facts.impl.persistent.scala

import java.io.StringWriter

import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.io.StandardTextWriter
import org.eclipse.imp.pdb.facts.`type`.Type

trait Value extends IValue {

  def t: Type
  
  def getType = t

  def isEqual(that: IValue) = this equals that

  final override def toString: String = {
    val stream = new StringWriter
    new StandardTextWriter write (this, stream)
    return stream toString
  }

}