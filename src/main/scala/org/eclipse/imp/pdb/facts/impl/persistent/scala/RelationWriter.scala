/*******************************************************************************
 * Copyright (c) 2012-2013 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *    * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI
 ******************************************************************************/
package org.eclipse.imp.pdb.facts.impl.persistent.scala

import org.eclipse.imp.pdb.facts.`type`._
import org.eclipse.imp.pdb.facts.`type`.TypeFactory


class RelationWriter(et: Type)
	extends SetWriter(et) {

	require(et isTuple)

	override def done = Set(et, xs)

}

sealed class RelationWriterWithTypeInference()
	extends RelationWriter(TypeFactory.getInstance voidType) {

	override def done = Set(`type` lub xs, xs)

}
