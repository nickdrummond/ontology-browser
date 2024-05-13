package org.ontbrowser.www.kit;

import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import javax.annotation.Nonnull;
import java.net.URI;
import java.util.Comparator;
import java.util.Optional;
import java.util.Set;

public interface OWLHTMLKit extends ActiveOntologyProvider {

    URLScheme getURLScheme();

    /**
     * Get the ontologies used for reasoning
     * @return imports closure of the current active ontology (plus meta ontology if it exists)
     */
    Set<OWLOntology> getActiveOntologies();

    Set<OWLOntology> getOntologies();

    void setActiveOntology(@Nonnull OWLOntology ont);

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

    String render(OWLObject owlObject);

    String getOntIRI(OWLOntology ontology);

    OntologyIRIShortFormProvider getOntologyShortFormProvider();

    OWLOntology getRootOntology();

    void setLabelParams(URI labelURI, String labelLang);
}
