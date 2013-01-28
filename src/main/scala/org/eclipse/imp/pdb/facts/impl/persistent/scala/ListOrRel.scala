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

import org.eclipse.imp.pdb.facts.IList
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

object ListOrRel {
  def apply[ListOrRel <: IList](et: Type, xs: List.Coll): ListOrRel = {
    val elementType = if (xs isEmpty) TypeFactory.getInstance voidType else et
    if (elementType isTupleType) ListRelation(elementType, xs) else List(elementType, xs)    
  }.asInstanceOf[ListOrRel]
}