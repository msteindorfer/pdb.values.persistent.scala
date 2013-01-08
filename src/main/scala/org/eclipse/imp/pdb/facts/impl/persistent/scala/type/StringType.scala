package org.eclipse.imp.pdb.facts.impl.persistent.scala.`type`

import org.eclipse.imp.pdb.facts.IValueFactory
import org.eclipse.imp.pdb.facts.`type`.ITypeVisitor
import org.eclipse.imp.pdb.facts.`type`.Type

case object StringType extends Type {

	def getInstance = this
  
    override def isStringType: Boolean = true
    
    override def equals(other: Any): Boolean = other match {
      case that: AnyRef => this eq that
    }

    override def hashCode = 94903

    override def toString: String = "str"

    override def accept[T](v: ITypeVisitor[T]): T = v visitString this
          
    override def make(factory: IValueFactory, content: String) = factory.string(content)
  
}