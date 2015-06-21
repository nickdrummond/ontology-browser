package org.coode.www.cloud;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Set;

public interface OWLCloudModel<O extends OWLObject> extends CloudModel<O> {

    void setOntologies(Set<OWLOntology> onts);

    Set<OWLOntology> getOntologies();
}
