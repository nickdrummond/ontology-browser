package org.ontbrowser.www.backend;

import org.ontbrowser.www.controller.AppStatus;
import org.ontbrowser.www.url.RestURLScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Comparator;

public interface BackendContext {
    OWLOntology getRootOntology();

    OWLOntology getOntologyFor(String ontId);

    OntologyIRIShortFormProvider getOntologySFP();

    ShortFormProvider getShortFormProvider();

    ShortFormProvider getShortFormProvider(OWLAnnotationProperty prop);

    OWLDataFactory getOWLDataFactory();

    OWLReasoner getToldReasoner(OWLOntology ont);

    EntityIdLookup lookup();

    IRIShortFormProvider getIriShortFormProvider();

    Comparator<OWLObject> getComparator();

    OWLEntityFinder getFinder();

    OWLEntityChecker getOWLEntityChecker();

    AppStatus getStatus();

    default URLScheme getURLScheme(){
        return new RestURLScheme(getIriShortFormProvider());
    };
}
