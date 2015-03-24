/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObject;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 *
 * Handy version for getting access to the server
 */
public abstract class AbstractOWLDocDoclet<O extends OWLObject> extends AbstractHTMLDoclet<O>{

    private OWLHTMLKit kit;


    public AbstractOWLDocDoclet(OWLHTMLKit kit) {
        this.kit = kit;
    }

    protected final OWLHTMLKit getOWLHTMLKit(){
        return kit;
    }

    protected boolean isSingleFrameNavigation() {
        return true;
    }
}
