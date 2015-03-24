/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.OWLDataProperty;
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
public class OWLDataPropertyHierarchyProvider extends AbstractPropertyHierarchyProvider<OWLDataProperty> {

    public OWLDataPropertyHierarchyProvider(OWLServer server) {
        super(server);
    }

    @Override
    protected OWLDataProperty getTopProperty() {
        return getServer().getOWLOntologyManager().getOWLDataFactory().getOWLTopDataProperty();
    }

    protected Set<OWLDataProperty> getPropertiesInSignature(OWLOntology ont) {
        return ont.getDataPropertiesInSignature();
    }

    public Class<? extends OWLDataProperty> getNodeClass() {
        return OWLDataProperty.class;
    }
}