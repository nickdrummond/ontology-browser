package org.coode.www.service;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

@Service
public class OWLClassesService {

    // TODO need to index the entities by ID
    public OWLClass getOWLClassFor(final String classId, final OWLHTMLKit kit) throws NotFoundException {
        OWLClass owlThing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();
        if (getIdFor(owlThing).equals(classId)) {
            return owlThing;
        }
        OWLClass owlNothing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLNothing();
        if (getIdFor(owlNothing).equals(classId)) {
            return owlNothing;
        }

        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLClass owlClass: ont.getClassesInSignature()) {
                if (getIdFor(owlClass).equals(classId)){
                    return owlClass;
                }
            }
        }
        throw new NotFoundException("OWLClass", classId);
    }

    public String getIdFor(final OWLClass owlClass) {
        return String.valueOf(owlClass.getIRI().hashCode());
    }
}
