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
import org.eclipse.imp.pdb.facts.IValue
import org.eclipse.imp.pdb.facts.ISet
import org.eclipse.imp.pdb.facts.ITuple
import org.eclipse.imp.pdb.facts.`type`.Type
import org.eclipse.imp.pdb.facts.`type`.TypeFactory

class RelationWriter(t: Type) extends SetWriter(t) with IRelationWriter {
  
  override def done = Relation(t, xs)

}

class RelationWriterWithTypeInference() extends SetWriterWithTypeInference() with IRelationWriter {

  override def done = Relation(t, xs)

}
  
