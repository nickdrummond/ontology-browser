package org.coode.www.controller;

import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.impl.PropertyComparator;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.QueryTimeoutException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.OWLObjectWithOntology;
import org.coode.www.model.DLQuery;
import org.coode.www.model.QueryType;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.ParserService;
import org.coode.www.service.ReasonerService;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import uk.co.nickdrummond.parsejs.ParseException;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

@Controller
@RequestMapping(value="/dlquery")
public class DLQueryController extends ApplicationController {

    @Autowired
    private ParserService parserService;

    @Autowired
    private ReasonerService reasonerService;

    @RequestMapping(method=RequestMethod.GET)
    public String dlQuery(
            @RequestParam(required = false, defaultValue = "") final String expression,
            @RequestParam(required = false, defaultValue = "") final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(required = false, defaultValue = "instances", name = "query") final QueryType queryType,
            final Model model) throws OntServerException, ParseException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();

        if (!expression.isEmpty()) {
            OWLClassExpression owlClassExpression = parserService.getOWLClassExpression(expression, df, checker);
            reasonerService.asyncQuery(new DLQuery(owlClassExpression, queryType));
        }

        if (!minus.isEmpty()) {
            reasonerService.asyncQuery(new DLQuery(parserService.getOWLClassExpression(minus, df, checker), queryType));
        }

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

        model.addAttribute("reasonerName", reasonerService.getReasoner().getReasonerName());
        model.addAttribute("reasoningOntology", reasonerService.getReasoningActiveOnt());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("ontologies", kit.getOntologies());
        model.addAttribute("expression", expression);
        model.addAttribute("minus", minus);
        model.addAttribute("order", order);
        model.addAttribute("query", queryType);
        model.addAttribute("queries", QueryType.values());

        return "dlquery";
    }

    @RequestMapping(value="results",method=RequestMethod.GET)
    public String getResults(
            @RequestParam final String expression,
            @RequestParam(required = false) final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(name="query") final QueryType queryType,
            final Model model) throws OntServerException, QueryTimeoutException, ParserException {

        try {
            OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
            OWLEntityChecker checker = kit.getOWLEntityChecker();

            DLQuery query = new DLQuery(parserService.getOWLClassExpression(expression, df, checker), queryType);
            reasonerService.asyncQuery(query);

            if (minus != null && !minus.isEmpty()) {
                DLQuery minusQuery = new DLQuery(parserService.getOWLClassExpression(minus, df, checker), queryType);
                reasonerService.asyncQuery(minusQuery);
            }

            Comparator<OWLObject> c = kit.getComparator();

            if (order != null && !order.isEmpty()) {
                OWLReasoner r = reasonerService.getReasoner();
                OWLDataProperty orderProperty = kit.getOWLEntityChecker().getOWLDataProperty(order);
                if (orderProperty != null) {
                    System.out.println("Sorting by: " + orderProperty);
                    c = new PropertyComparator(orderProperty, c, r);
                }
            }

            Set<OWLEntity> results = reasonerService.getCachedResults(query);

            if (minus != null && !minus.isEmpty()) {
                Set<OWLEntity> minusResults = reasonerService.getCachedResults(new DLQuery(parserService.getOWLClassExpression(minus, df, checker), queryType));
                // Wish there was a neater immutable version of this
                Set<OWLEntity> resultsCopy = new HashSet<>(results);
                resultsCopy.removeAll(minusResults);
                results = resultsCopy;
            }
            Characteristic resultsCharacteristic = buildCharacteristic(queryType.name(), results, c);

            OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

            model.addAttribute("results", resultsCharacteristic);
            model.addAttribute("mos", owlRenderer);

            return "base :: results";
        } catch (ExecutionException e) {
            throw new OntServerException(e);
        } catch (InterruptedException | TimeoutException e) {
            throw new QueryTimeoutException();
        }
    }

    @RequestMapping(value = "/ac", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String autocompleteOWLClassExpression(
            @RequestParam String expression) throws OntServerException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();
        OWLEntityFinder finder = kit.getFinder();
        ShortFormProvider sfp = kit.getShortFormProvider();

        return parserService.autocomplete(expression, df, checker, finder, sfp).toString();
    }

    // TODO return the actual ParseResult or an XML rendering of the parse exception
    @RequestMapping(value = "/parse", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String parseOWLClassExpression(
            @RequestParam String expression) throws OntServerException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();

        try {
            return parserService.parse(expression, df, checker).toString();
        } catch (ParseException e) {
            return e.toString();
        }
    }

    private OWLOntology getDeclarationOntology(OWLEntity e, OWLHTMLKit kit) {
        OWLDeclarationAxiom decl = kit.getOWLOntologyManager().getOWLDataFactory().getOWLDeclarationAxiom(e);
        OWLOntology rActiveOnt = reasonerService.getReasoningActiveOnt();
        for (OWLOntology o : rActiveOnt.getImportsClosure()) {
            if (o.containsAxiom(decl)) {
                return o;
            }
        }
        return rActiveOnt;
    }

    private Characteristic buildCharacteristic(String name, Set<OWLEntity> results, Comparator<OWLObject> comp) {
        return new Characteristic(null, name,
                results.stream()
                        .sorted(comp)
                        .map(e -> new OWLObjectWithOntology(e, getDeclarationOntology(e, kit)))
                        .collect(Collectors.toList()));
    }
}
