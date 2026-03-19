package org.ontbrowser.www.backend;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

public interface BackendContext {
    OWLOntology getRootOntology();

    OWLOntology getOntologyFor(String ontId);

    OntologyIRIShortFormProvider getOntologySFP();

    ShortFormProvider getShortFormProvider();

    OWLDataFactory getOWLDataFactory();

    OWLReasoner getToldReasoner(OWLOntology ont);
}
