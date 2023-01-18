package org.coode.www.service;

import com.google.common.collect.ImmutableSet;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.DLQuery;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.*;

@Service
public class ReasonerService {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    private final OWLHTMLKit kit;

    @Value("${reasoning.root.iri}")
    private String reasoningRootIRI;

    /**
     * Use a small thread pool for all reasoner queries to prevent overloading the server.
     */
    private final ExecutorService es;
    /**
     * Cache the last x results in futures, allowing the result to continue to be computed regardless
     * of the server or client timing out - long queries can then be retrieved on future requests.
     */
    private final Map<DLQuery, Future<Set<OWLEntity>>> cache;

    private final ReasonerFactoryService reasonerFactoryService;

    public ReasonerService(OWLHTMLKit kit,
                           ExecutorService es,
                           Map<DLQuery, Future<Set<OWLEntity>>> cache,
                           ReasonerFactoryService reasonerFactoryService) {
        this.kit = kit;
        this.es = es;
        this.cache = cache;
        this.reasonerFactoryService = reasonerFactoryService;
    }

    public synchronized Future<Set<OWLEntity>> asyncQuery(@NonNull final DLQuery query) {
        Future<Set<OWLEntity>> setFuture = cache.get(query);
        if (setFuture == null) {
            setFuture = computeResults(query);
            cache.put(query, setFuture);
        }
        return setFuture;
    }

    public Set<OWLEntity> getCachedResults(final DLQuery query) throws ExecutionException, InterruptedException, TimeoutException {
        return cache.get(query).get(10, TimeUnit.SECONDS);
    }

    public OWLReasoner getReasoner() {
        return reasonerFactoryService.getReasoner(getReasoningActiveOnt());
    }

    public OWLOntology getReasoningActiveOnt() {
        return kit.getOntologyForIRI(IRI.create(reasoningRootIRI)).orElseThrow();
    }

    private Future<Set<OWLEntity>> computeResults(final DLQuery query) {
        return es.submit(() -> {
            long start = System.currentTimeMillis();

            OWLOntology reasoningOnt = getReasoningActiveOnt();

            OWLReasoner r = reasonerFactoryService.getReasoner(reasoningOnt);

            Set<OWLEntity> results = getResults(query, r);

            logger.debug(query.getQueryType() + " of \"" + query.getOwlClassExpression() + "\": " + results.size() + " results in " + (System.currentTimeMillis()-start) + "ms");

            return results;
        });
    }

    private Set<OWLEntity> getResults(final DLQuery query, final OWLReasoner r) {
        switch(query.getQueryType()){
            case equivalents:  return ImmutableSet.copyOf(r.getEquivalentClasses(query.getOwlClassExpression()).getEntities());
            case subclasses:   return ImmutableSet.copyOf(r.getSubClasses(query.getOwlClassExpression(), true).getFlattened());
            case descendants:  return ImmutableSet.copyOf(r.getSubClasses(query.getOwlClassExpression(), false).getFlattened());
            case superclasses: return ImmutableSet.copyOf(r.getSuperClasses(query.getOwlClassExpression(), true).getFlattened());
            case ancestors:    return ImmutableSet.copyOf(r.getSuperClasses(query.getOwlClassExpression(), false).getFlattened());
            case instances:    return ImmutableSet.copyOf(r.getInstances(query.getOwlClassExpression(), false).getFlattened());
        }
        throw new IllegalArgumentException("Unexpected query: " + query);
    }
}
