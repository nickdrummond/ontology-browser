package org.ontbrowser.www.feature.dlquery;

import com.google.common.collect.ImmutableSet;
import org.ontbrowser.www.kit.RestartListener;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.reasoner.ReasonerFactoryService;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
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
public class ReasonerService implements RestartListener {

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
            @Autowired ExecutorService es,
            @Autowired Map<DLQuery, Future<Set<OWLEntity>>> cache,
            @Autowired ReasonerFactoryService reasonerFactoryService) {
        this.kit = kit;
        kit.registerListener(this);
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

    public synchronized Set<OWLEntity> query(@NonNull final DLQuery query) throws ExecutionException, InterruptedException {
        return asyncQuery(query).get();
    }

    public Set<OWLEntity> getCachedResults(final DLQuery query) throws ExecutionException, InterruptedException, TimeoutException {
        return cache.get(query).get(10, TimeUnit.SECONDS);
    }

    public OWLReasoner getReasoner() {
        return reasonerFactoryService.getReasoner(getReasoningActiveOnt());
    }

    public OWLOntology getReasoningActiveOnt() {
        return kit.getOntologyForIRI(IRI.create(reasoningRootIRI)).orElse(kit.getRootOntology());
    }

    private Future<Set<OWLEntity>> computeResults(final DLQuery query) {
        return es.submit(() -> {
            long start = System.nanoTime();

            var reasoningOnt = getReasoningActiveOnt();

            var r = reasonerFactoryService.getReasoner(reasoningOnt);

            Set<OWLEntity> results = getResults(query, r);

            long t = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);

            if (logger.isInfoEnabled()) {
                String expr = kit.getStringRenderer().render(query.owlClassExpression());
                logger.info("{} {} results in {} ms: {}", query.queryType(), results.size(), t, expr);
            }

            return results;
        });
    }

    private Set<OWLEntity> getResults(final DLQuery query, final OWLReasoner r) {
        var clsExpr = query.owlClassExpression();
        var entities = switch (query.queryType()) {
            case equivalents -> r.getEquivalentClasses(clsExpr).getEntities();
            case subclasses -> r.getSubClasses(clsExpr, true).getFlattened();
            case descendants -> r.getSubClasses(clsExpr, false).getFlattened();
            case superclasses -> r.getSuperClasses(clsExpr, true).getFlattened();
            case ancestors -> r.getSuperClasses(clsExpr, false).getFlattened();
            case instances -> r.getInstances(clsExpr, false).getFlattened();
        };
        return ImmutableSet.copyOf(entities);
    }

    @Override
    public void onRestart() {
        cache.clear();
        reasonerFactoryService.clear();
    }
}
