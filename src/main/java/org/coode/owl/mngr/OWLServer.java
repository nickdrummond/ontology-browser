package org.coode.owl.mngr;

import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.net.URI;
import java.util.Comparator;
import java.util.Map;
import java.util.Set;

public interface OWLServer extends ActiveOntologyProvider {

    OWLOntology getActiveOntology();
    /**
     * Get the ontologies used for reasoning
     * @return imports closure of the current active ontology (plus meta ontology if it exists)
     */
    Set<OWLOntology> getActiveOntologies();

    Set<OWLOntology> getOntologies();

    void setActiveOntology(OWLOntology ont);

    OWLOntology loadOntology(URI ontPhysicalURI) throws OWLOntologyCreationException;

    void loadOntologies(Map<IRI, IRI> ontMap);

    OWLOntology reloadOntology(OWLOntology ontology) throws OWLOntologyCreationException;

    void removeOntology(OWLOntology ont);

    /**
     * First get an ontology with a matching version IRI if one exists.
     * If not, get an ontology with a matching ontology IRI.
     * @param iri the IRI
     * @return an Ontology if one matches or null if none is found
     */
    OWLOntology getOntologyForIRI(IRI iri);

    OWLOntologyManager getOWLOntologyManager();

    OWLReasoner getOWLReasoner();

    <N extends OWLObject> HierarchyProvider<N> getHierarchyProvider(Class<N> cls);

    Comparator<OWLObject> getComparator();

    OWLEntityFinder getFinder();

    OWLEntityChecker getOWLEntityChecker();

    ShortFormProvider getShortFormProvider();

    OntologyIRIShortFormProvider getOntologyShortFormProvider();

    OWLClassExpressionParser getClassExpressionParser(String type);

    void registerDescriptionParser(String syntax, OWLClassExpressionParser parser);

    Set<String> getSupportedSyntaxes();

    ServerOptionsAdapter<ServerProperty> getProperties();

    void resetProperties();


    /**
     * Get rid of all caches (such as renderings) and clear the reasoner.
     * Do not clear the loaded ontologies - this is done with clearOntologies
     */
    void clear();

    void clearOntologies();
    
    void dispose();

    boolean isDead();

    OWLOntology getRootOntology();
}