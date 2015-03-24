/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 *
 * HTMLDoclets are nestable views on a user object
 */
public interface NestedHTMLDoclet<O> extends HTMLDoclet<O> {

    /**
     * Enforces that all subdoclets are of the same type.
     * We may need to relax this for composite pages that show different things
     * @param doclet that contains the same type of user object as the parent
     */
    void addDoclet(HTMLDoclet<O> doclet);
    
    void addDoclet(HTMLDoclet<O> doclet, int index);

    void removeDoclet(HTMLDoclet doclet);

    HTMLDoclet getDoclet(String id);

    int getSubDocletCount();

    /**
     * Remove all sub doclets
     */
    void clear();

    int indexOf(HTMLDoclet doclet);
}
