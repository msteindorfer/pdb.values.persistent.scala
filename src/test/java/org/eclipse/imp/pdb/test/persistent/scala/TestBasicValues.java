package org.eclipse.imp.pdb.test.persistent.scala;

import org.eclipse.imp.pdb.facts.impl.persistent.scala.ValueFactory;
import org.eclipse.imp.pdb.test.BaseTestBasicValues;

public class TestBasicValues extends BaseTestBasicValues {
	
	@Override
	protected void setUp() throws Exception {
		super.setUp(new ValueFactory());
	}
}
