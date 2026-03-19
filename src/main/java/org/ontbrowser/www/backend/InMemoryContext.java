package org.ontbrowser.www.backend;

import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import org.springframework.web.server.ResponseStatusException;

@Component
@ConditionalOnProperty(name = "ontology.backend", havingValue = "memory", matchIfMissing = true)
@RequestScope(proxyMode = ScopedProxyMode.TARGET_CLASS) // for ReadOnlyOntologyControllerAdvice
public class InMemoryContext implements BackendContext {

    private final OWLHTMLKit kit; // injected singleton Kit
    private final ReasonerFactoryService reasonerFactoryService;

    public InMemoryContext(OWLHTMLKit kit, ReasonerFactoryService reasonerFactoryService) {
        this.kit = kit;
        this.reasonerFactoryService = reasonerFactoryService;
    }

    @Override
    public OWLOntology getRootOntology() {
        return kit.getRootOntology();
    }

    // TODO duplicates OWLOntologiesService.getOntologyFor, consider refactor
    @Override
    public OWLOntology getOntologyFor(String ontId) {
        for (OWLOntology ont : kit.getOntologies()){
            if (getIdFor(ont).equals(ontId)){
                return ont;
            }
        }
        throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Ontology not found: " + ontId);
    }

    private String getIdFor(final OWLOntology ontology) {
        return String.valueOf(ontology.getOntologyID().hashCode());
    }

    @Override
    public OntologyIRIShortFormProvider getOntologySFP() {
        return kit.getOntologySFP();
    }

    @Override
    public ShortFormProvider getShortFormProvider() {
        return kit.getShortFormProvider();
    }

    @Override
    public OWLDataFactory getOWLDataFactory() {
        return kit.getOWLOntologyManager().getOWLDataFactory();
    }

    @Override
    public OWLReasoner getToldReasoner(OWLOntology ont) {
        return reasonerFactoryService.getToldReasoner(ont);
    }
}
