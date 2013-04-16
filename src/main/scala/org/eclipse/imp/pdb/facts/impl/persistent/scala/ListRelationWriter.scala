/*******************************************************************************
 * Copyright (c) 2012-2013 CWI
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

import org.eclipse.imp.pdb.facts.IListRelation
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

class ListRelationWriter(et: Type)
  extends ListWriter(et) {

  require (et isTupleType)
  
  override def done = List(et, emptyList ++ xs.result)
  
}

sealed class ListRelationWriterWithTypeInference()
  extends ListRelationWriter(TypeFactory.getInstance voidType) {
  
  override def done = {
    val zs = emptyList ++ xs.result ;
    List(`type` lub zs, zs)
  }  
  
}