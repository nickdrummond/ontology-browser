/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class OWLObjectPropertyHierarchyProvider extends AbstractPropertyHierarchyProvider<OWLObjectProperty> {

    public OWLObjectPropertyHierarchyProvider(OWLServer server) {
        super(server);
    }

    @Override
    protected OWLObjectProperty getTopProperty() {
        return getServer().getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty();
    }

    protected Set<OWLObjectProperty> getPropertiesInSignature(OWLOntology ont) {
        return ont.getObjectPropertiesInSignature();
    }

    public Class<? extends OWLObjectProperty> getNodeClass() {
        return OWLObjectProperty.class;
    }
}
