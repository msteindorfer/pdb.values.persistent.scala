package org.eclipse.imp.pdb.facts.impl.persistent

import org.eclipse.imp.pdb.facts.IValue

package object scala {
 
  type ListColl				= collection.immutable.Vector[IValue]
  val emptyList: ListColl	= collection.immutable.Vector.empty[IValue]

}