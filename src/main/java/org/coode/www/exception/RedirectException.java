/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.exception;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 11, 2008<br><br>
 *
 * A redirect can be relative to the current request page
 */
public class RedirectException extends OntServerException {

    private String page;

    public RedirectException(String page) {
        this.page = page;
    }

    public String getRedirectPage(){
        return page;
    }
}
