package org.coode.www.controller;

import java.util.*;

import org.apache.commons.collections4.map.LRUMap;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.impl.PropertyComparator;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.QueryTimeoutException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.OWLObjectWithOntology;
import org.coode.www.model.QueryType;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.ParserService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.ReasonerService;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import uk.co.nickdrummond.parsejs.ParseException;

import javax.servlet.http.HttpServletRequest;
import java.util.concurrent.*;
import java.util.stream.Collectors;

@Controller
@RequestMapping(value="/dlquery")
public class DLQueryController extends ApplicationController {

    @Value("${reasoning.root.iri}")
    private String reasoningRootIRI;

    @Autowired
    private ParserService service;

    @Autowired
    private ReasonerService reasonerService;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @Value("${reasoning.cache.count}")
    private int cacheCount;

    /**
     * Use a single thread for all of the reasoner queries to prevent overloading the server.
     */
    private final ExecutorService es = Executors.newFixedThreadPool(2);
    /**
     * Cache the last x results in futures, allowing the result to continue to be computed regardless
     * of the server or client timing out - long queries can then be retrieved on future requests.
     */
    private Map<String, Future<Set<OWLEntity>>> cache;

    private OWLOntology getReasoningActiveOnt() {
        return kit.getOntologyForIRI(IRI.create(reasoningRootIRI)).orElseThrow();
    }

    @RequestMapping(method=RequestMethod.GET)
    public String dlQuery(
            @RequestParam(required = false, defaultValue = "") final String expression,
            @RequestParam(required = false) final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(required = false, defaultValue = "instances") final QueryType query,
            final HttpServletRequest request) throws OntServerException, ParseException {
        return redirect(request);
    }

    synchronized private void preload(@NonNull final String expression,
                                      @NonNull final QueryType query) {
        if (cache == null) {
            cache = Collections.synchronizedMap(new LRUMap<>(cacheCount));
        }
        String key = expression + query.name();
        if (!expression.isEmpty() && cache.get(key) == null) {
            cache.put(key, computeResults(expression, query));
        }
    }

    private Future<Set<OWLEntity>> computeResults(final String expression,
                                                  final QueryType query) {
        return es.submit(() -> {
            long start = System.currentTimeMillis();

            OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
            OWLEntityChecker checker = kit.getOWLEntityChecker();

            OWLOntology reasoningOnt = getReasoningActiveOnt();

            OWLReasoner r = reasonerFactoryService.getReasoner(reasoningOnt);

            OWLClassExpression owlClassExpression = service.getOWLClassExpression(expression, df, checker);

            Set<OWLEntity> results = reasonerService.getResults(owlClassExpression, query, r);

            logger.debug(query + " of \"" + expression + "\": " + results.size() + " results in " + (System.currentTimeMillis()-start) + "ms");

            return results;
        });
    }

    private OWLOntology getDeclarationOntology(OWLEntity e, OWLHTMLKit kit) {
        OWLDeclarationAxiom decl = kit.getOWLOntologyManager().getOWLDataFactory().getOWLDeclarationAxiom(e);
        for (OWLOntology o : getReasoningActiveOnt().getImportsClosure()) {
            if (o.containsAxiom(decl)) {
                return o;
            }
        }
        return getReasoningActiveOnt();
    }

    @RequestMapping(value="results",method=RequestMethod.GET)
    public String getResults(
            @RequestParam(required = true) final String expression,
            @RequestParam(required = false) final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(required = true) final QueryType query,
            final HttpServletRequest request) throws OntServerException, QueryTimeoutException, ParserException {
        return redirect(request);
    }

    private Characteristic buildCharacteristic(String name, Set<OWLEntity> results, Comparator<OWLObject> comp) {
        return new Characteristic(null, name,
                results.stream()
                        .sorted(comp)
                        .map(e -> new OWLObjectWithOntology(e, getDeclarationOntology(e, kit)))
                        .collect(Collectors.toList()));
    }

    @RequestMapping(value = "/ac", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String autocompleteOWLClassExpression(
            @RequestParam(required = true) String expression,
            final HttpServletRequest request) throws OntServerException {
        return redirect(request);
    }

    // TODO return the actual ParseResult or an XML rendering of the parse exception
    @RequestMapping(value = "/parse", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String parseOWLClassExpression(
            @RequestParam(required = true) String expression,
            final HttpServletRequest request) throws OntServerException {
        return redirect(request);
    }

    @RequestMapping("/refresh")
    public String refresh(final HttpServletRequest request) {
        return redirect(request);
    }
}
