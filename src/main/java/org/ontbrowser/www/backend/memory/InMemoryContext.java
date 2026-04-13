package org.ontbrowser.www.backend.memory;

import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.backend.memory.kit.impl.RestartableKit;
import org.ontbrowser.www.controller.AppStatus;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.backend.OWLEntityFinder;
import org.ontbrowser.www.backend.memory.kit.OWLHTMLKit;
import org.ontbrowser.www.backend.EntityIdLookup;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import org.springframework.web.server.ResponseStatusException;

import java.util.Comparator;

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
    public ShortFormProvider getShortFormProvider(OWLAnnotationProperty prop) {
        return null;
    }

    @Override
    public OWLDataFactory getOWLDataFactory() {
        return kit.getOWLOntologyManager().getOWLDataFactory();
    }

    @Override
    public OWLReasoner getToldReasoner(OWLOntology ont) {
        return reasonerFactoryService.getToldReasoner(ont);
    }

    @Override
    public EntityIdLookup lookup() {
        return kit.lookup();
    }

    @Override
    public IRIShortFormProvider getIriShortFormProvider() {
        return kit.getIriShortFormProvider();
    }

    @Override
    public Comparator<OWLObject> getComparator() {
        return kit.getComparator();
    }

    @Override
    public OWLEntityFinder getFinder() {
        return kit.getFinder();
    }

    @Override
    public OWLEntityChecker getOWLEntityChecker() {
        return kit.getOWLEntityChecker();
    }

    @Override
    public AppStatus getStatus() {
        if (kit instanceof RestartableKit restartable) {
            return restartable.getStatus();
        }
        return new AppStatus(AppStatus.Status.UP);
    }
}
