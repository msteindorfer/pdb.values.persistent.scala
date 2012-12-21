package org.eclipse.imp.pdb.facts.impl.persistent.scala

import org.eclipse.imp.pdb.facts.IString

// TODO: make value class (i.e. extend from AnyVal)
abstract class StringValue extends IString {

	/**
	 * @return the Java string that this string represents
	 */
    def getValue: String;

    /**
     * Concatenates two strings
     * @param other
     * @return
     */
    def concat(other: IString): IString;
    
    /**
     * Reverses a string
     */
    def reverse: IString;

    /**
     * Computes the length of the string 
     * @return amount of Unicode characters 
     */
    def length: Int;
    
    /**
     * Computes a substring
     *  
     * @param start the inclusive start index
     * @param end   the exclusive end index
     */
    def substring(start: Int, end: Int): IString;
    
    /**
     * Computes a substring
     *  
     * @param start the inclusive start index
     */
    def substring(start: Int): IString ;
    
    /**
     * Compares two strings lexicographically
     * @param other
     * @return -1 if receiver is less than other, 0 is receiver is equal, 1 if receiver is larger
     */
    def compare(other: IString): Int;
    
    /**
     * Returns the Unicode character at the given index.
     * @param index
     * @return
     */
    def charAt(index: Int): Int;  
  
}