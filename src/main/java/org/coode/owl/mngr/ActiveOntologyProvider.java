package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.OWLOntology;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Mar 24, 2011<br><br>
 */
public interface ActiveOntologyProvider {

    OWLOntology getActiveOntology();

    void addActiveOntologyListener(Listener l);

    void removeActiveOntologyListener(Listener l);

    public interface Listener{
        void activeOntologyChanged(OWLOntology ont);
    }
}
