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

import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

object SetOrRel {
  def apply[SetOrRel <: ISet](et: Type, xs: Set.Coll): SetOrRel = {
    val elementType = if (xs isEmpty) TypeFactory.getInstance voidType else et
    if (elementType isTupleType) Relation(elementType, xs) else Set(elementType, xs)    
  }.asInstanceOf[SetOrRel]
}