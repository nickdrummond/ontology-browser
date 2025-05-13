package org.ontbrowser.www.util;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Comparator;

public class OWLObjectComparator implements Comparator<OWLObject> {

    private final ShortFormProvider sfp;

    public OWLObjectComparator(final ShortFormProvider sfp) {
        this.sfp = sfp;
    }

    public int compare(OWLObject o1, OWLObject o2) {
        if (o1.equals(o2)) {
            return 0;
        }
        else if (o1.isTopEntity() && !o2.isTopEntity()){ // owl:Thing is always first
            return -1;
        }
        else if (o2.isTopEntity() && !o1.isTopEntity()){
            return 1;
        }
        else if ((o1 instanceof OWLEntity entity1) && (o2 instanceof OWLEntity entity2)){
            String ren1 = sfp.getShortForm(entity1);
            String ren2 = sfp.getShortForm(entity2);
            return ren1.compareToIgnoreCase(ren2);
        }
        else if (o1 instanceof OWLEntity){ // named things always come before anonymous things
            return -1;
        }
        else if (o2 instanceof OWLEntity){ // named things always come before anonymous things
            return 1;
        }
        else{ // we don't care about the order of anonymous things - use default rendering for now
            return o1.compareTo(o2);
        }
    }
}
