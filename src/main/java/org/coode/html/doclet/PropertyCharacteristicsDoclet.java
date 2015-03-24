/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class PropertyCharacteristicsDoclet<O extends OWLProperty> extends AbstractOWLElementsDoclet<O, OWLPropertyAxiom> {

    public PropertyCharacteristicsDoclet(OWLHTMLKit kit) {
        super("Property Characteristics", ElementsDoclet.Format.list, kit);
    }

    protected Collection<OWLPropertyAxiom> getAssertedElements(Set<OWLOntology> onts) {
        Set<OWLPropertyAxiom> axioms = new HashSet<OWLPropertyAxiom>();
        O prop = getUserObject();
        for (OWLOntology ont : onts){
            if (prop instanceof OWLObjectProperty){
                for (OWLObjectPropertyAxiom ax : ont.getAxioms((OWLObjectProperty)prop)){
                    if (ax instanceof OWLObjectPropertyCharacteristicAxiom){
                        axioms.add(ax);
                    }
                }
            }
            else if (prop instanceof OWLDataProperty){
                for (OWLDataPropertyAxiom ax : ont.getAxioms((OWLDataProperty)prop)){
                    if (ax instanceof OWLDataPropertyCharacteristicAxiom){
                        axioms.add((OWLDataPropertyCharacteristicAxiom)ax);
                    }
                }
            }
        }
        return axioms;
    }
}
