package org.coode.www.exception;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 4, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class OntServerException extends Exception {

    public OntServerException(){
    }

    public OntServerException(String s) {
        super(s);
    }

    public OntServerException(Exception e) {
        super(e);
    }

    public OntServerException(Throwable e) {
        super(e);
    }
}
