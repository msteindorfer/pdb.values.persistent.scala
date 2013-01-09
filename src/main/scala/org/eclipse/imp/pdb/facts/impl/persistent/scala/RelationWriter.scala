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

import org.eclipse.imp.pdb.facts.IRelationWriter
import org.eclipse.imp.pdb.facts.IRelation
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.`type`._
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

import collection.immutable.Set.empty
import collection.JavaConversions.mapAsScalaMap
import collection.JavaConversions.iterableAsScalaIterable

class RelationWriter(et: Type)
  extends SetWriter(et) 
  with IRelationWriter {

  require (et isTupleType)
  
  override def done: IRelation = SetOrRel(et, xs)

}

sealed class RelationWriterWithTypeInference() 
  extends RelationWriter(TypeFactory.getInstance voidType)
  with IRelationWriter {
  
  override def done: IRelation = {
    val zs = empty ++ xs ;
    SetOrRel(`type` lub zs, zs)
  }
  
}
