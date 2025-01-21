package org.ontbrowser.www.kit;

import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.net.URI;
import java.util.Comparator;
import java.util.Optional;
import java.util.Set;

public interface OWLHTMLKit {

    URLScheme getURLScheme();

    Set<OWLOntology> getOntologies();

    /**
     * First get an ontology with a matching version IRI if one exists.
     * If not, get an ontology with a matching ontology IRI.
     * @param iri the IRI
     * @return an Ontology if one matches or null if none is found
     */
    Optional<OWLOntology> getOntologyForIRI(IRI iri);

    OWLOntologyManager getOWLOntologyManager();

    Comparator<OWLObject> getComparator();

    OWLEntityFinder getFinder();

    OWLEntityChecker getOWLEntityChecker();

    ShortFormProvider getShortFormProvider();

    MOSStringRenderer getStringRenderer();

    OntologyIRIShortFormProvider getOntologySFP();

    OWLOntology getRootOntology();

    void setLabelParams(URI labelURI, String labelLang);
}
