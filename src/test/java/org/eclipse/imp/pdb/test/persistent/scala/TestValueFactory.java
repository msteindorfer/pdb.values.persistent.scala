/*******************************************************************************
* Copyright (c) 2007 IBM Corporation.
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License v1.0
* which accompanies this distribution, and is available at
* http://www.eclipse.org/legal/epl-v10.html
*
* Contributors:
*    Robert Fuhrer (rfuhrer@watson.ibm.com) - initial API and implementation

*******************************************************************************/

package org.eclipse.imp.pdb.test.persistent.scala;

import org.eclipse.imp.pdb.facts.impl.persistent.scala.ValueFactory;
import org.eclipse.imp.pdb.test.BaseTestValueFactory;


public class TestValueFactory extends BaseTestValueFactory {

	@Override
	protected void setUp() throws Exception {
		super.setUp(new ValueFactory());
	}
}
