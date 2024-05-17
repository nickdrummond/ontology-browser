package org.ontbrowser.www.service;

import com.google.common.collect.ImmutableSet;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.DLQuery;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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

    public ReasonerService(
            @Autowired OWLHTMLKit kit,
            @Autowired     ExecutorService es,
            @Autowired     Map<DLQuery, Future<Set<OWLEntity>>> cache,
            @Autowired     ReasonerFactoryService reasonerFactoryService) {
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
        return kit.getOntologyForIRI(IRI.create(reasoningRootIRI)).orElse(kit.getActiveOntology());
    }

    private Future<Set<OWLEntity>> computeResults(final DLQuery query) {
        return es.submit(() -> {
            long start = System.nanoTime();

            OWLOntology reasoningOnt = getReasoningActiveOnt();

            OWLReasoner r = reasonerFactoryService.getReasoner(reasoningOnt);

            Set<OWLEntity> results = getResults(query, r);

            long t = TimeUnit.NANOSECONDS.toMillis(System.nanoTime()-start);

            logger.info(query.getQueryType() + " " + results.size() + " results in " + t + "ms: " + kit.render(query.getOwlClassExpression()));

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
