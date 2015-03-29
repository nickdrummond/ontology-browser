package org.coode.www.service;

import org.coode.html.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.OWLOntology;

public class OWLObjectIdMapper {

    private final OWLHTMLKit kit;

    public OWLObjectIdMapper(OWLHTMLKit kit) {
        this.kit = kit;
    }

    public OWLOntology getOntologyFor(final String id) throws NotFoundException {
        for (OWLOntology ont : kit.getOWLServer().getOntologies()){
            if (String.valueOf(ont.getOntologyID().hashCode()).equals(id)){
                return ont;
            }
        }
        throw new NotFoundException("Ontology", id);
    }

    public String getIdFor(final OWLOntology ontology) {
        return String.valueOf(ontology.getOntologyID().hashCode());
    }
}
