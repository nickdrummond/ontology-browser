package org.coode.owl.util;

import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Comparator;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 18, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class OWLObjectComparator<E extends OWLObject> implements Comparator<E> {

    private OWLServer server;

    public OWLObjectComparator(OWLServer server) {
        if (server == null){
            throw new NullPointerException("Server may not be null");
        }
        this.server = server;
    }

    public int compare(OWLObject o1, OWLObject o2) {
        if (o1 instanceof OWLEntity){
            if (o2 instanceof OWLEntity){
                
                // owl:Thing is always first
                final OWLClass thing = server.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
                if (o1.equals(thing) && !o2.equals(thing)){
                    return -1;
                }
                else if (o2.equals(thing) && !o1.equals(thing)){
                    return 1;
                }

                String ren1 = server.getShortFormProvider().getShortForm((OWLEntity)o1);
                String ren2 = server.getShortFormProvider().getShortForm((OWLEntity)o2);

                return ren1.compareToIgnoreCase(ren2);
            }
            else{ // named things always come before anonymous things
                return -1;
            }
        }
        else{
            if (o2 instanceof OWLEntity){ // named things always come before anonymous things
                return 1;
            }
            else{ // we don't care about the order of anonymous things - use default rendering for now
                return o1.compareTo(o2);
            }
        }
    }
}
