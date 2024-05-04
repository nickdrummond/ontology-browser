package org.ontbrowser.www.controller;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.ontbrowser.www.util.PropertyComparator;
import org.ontbrowser.www.exception.OntServerException;
import org.ontbrowser.www.exception.QueryTimeoutException;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.DLQuery;
import org.ontbrowser.www.model.QueryType;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.ParserService;
import org.ontbrowser.www.service.ReasonerService;
import org.ontbrowser.www.model.paging.PageData;
import org.ontbrowser.www.util.OWLUtils;
import org.ontbrowser.www.util.Utils;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import org.springframework.web.util.UriComponentsBuilder;
import uk.co.nickdrummond.parsejs.ParseException;

import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.concurrent.*;

@Controller
@RequestMapping(value= DLQueryController.PATH)
public class DLQueryController extends ApplicationController {

    private static final Logger log = LoggerFactory.getLogger(DLQueryController.class);

    public static final String PATH = "/dlquery";

    @Autowired
    private ParserService parserService;

    @Autowired
    private ReasonerService reasonerService;

    @SuppressWarnings("SameReturnValue")
    @GetMapping
    public String dlQuery(
            @RequestParam(required = false, defaultValue = "") final String expression,
            @RequestParam(required = false, defaultValue = "") final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(required = false, defaultValue = "instances", name = "query") final QueryType queryType,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            final HttpServletRequest request,
            final Model model) {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();

        if (!expression.isEmpty()) {
            OWLClassExpression owlClassExpression = parserService.getOWLClassExpression(expression, df, checker);
            reasonerService.asyncQuery(new DLQuery(owlClassExpression, queryType));
        }

        if (!minus.isEmpty()) {
            reasonerService.asyncQuery(new DLQuery(parserService.getOWLClassExpression(minus, df, checker), queryType));
        }

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(kit.getActiveOntology());

        model.addAttribute("reasonerName", reasonerService.getReasoner().getReasonerName());
        model.addAttribute("reasoningOntology", reasonerService.getReasoningActiveOnt());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("ontologies", kit.getOntologies());
        model.addAttribute("expression", expression);
        model.addAttribute("minus", minus);
        model.addAttribute("order", order);
        model.addAttribute("query", queryType);
        model.addAttribute("queries", QueryType.values());
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));

        return "dlquery";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="results")
    public String getResults(
            @RequestParam final String expression,
            @RequestParam(required = false) final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(name="query") final QueryType queryType,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            final HttpServletRequest request,
            final Model model) throws OntServerException, QueryTimeoutException, ParserException {

        try {
            OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
            OWLEntityChecker checker = kit.getOWLEntityChecker();

            DLQuery query = new DLQuery(parserService.getOWLClassExpression(expression, df, checker), queryType);
            reasonerService.asyncQuery(query);

            DLQuery minusQuery = null;
            if (minus != null && !minus.isEmpty()) {
                minusQuery = new DLQuery(parserService.getOWLClassExpression(minus, df, checker), queryType);
                reasonerService.asyncQuery(minusQuery);
            }

            Comparator<OWLObject> c = kit.getComparator();

            if (order != null && !order.isEmpty()) {
                OWLReasoner r = reasonerService.getReasoner();
                OWLDataProperty orderProperty = kit.getOWLEntityChecker().getOWLDataProperty(order);
                if (orderProperty != null) {
                    log.debug("Sorting by: {}", orderProperty);
                    c = new PropertyComparator(orderProperty, c, r);
                }
            }

            Set<OWLEntity> results = reasonerService.getCachedResults(query);

            if (minus != null && !minus.isEmpty()) {
                results = Utils.subtract(reasonerService.getCachedResults(minusQuery), results);
            }

            Characteristic resultsCharacteristic = buildCharacteristic(queryType.name(), results, c, start, pageSize);

            // TODO update scheme to render links with the entity as a param
            OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(kit.getActiveOntology());

            // Target links to parent page for fragment
            UriComponentsBuilder uriBuilder = ServletUriComponentsBuilder.fromCurrentRequest()
                    .replacePath(PATH)
                    .replaceQueryParam("expression", expression); // as it gets double encoded otherwise

            model.addAttribute("urlBuilder", uriBuilder);
            model.addAttribute("results", resultsCharacteristic);
            model.addAttribute("mos", owlRenderer);
            model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));

            return "base :: results";
        } catch (ExecutionException e) {
            throw new OntServerException(e);
        } catch (InterruptedException | TimeoutException e) {
            throw new QueryTimeoutException();
        }
    }

    @GetMapping(value = "/ac", produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String autocompleteOWLClassExpression(
            @RequestParam String expression) {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();
        OWLEntityFinder finder = kit.getFinder();
        ShortFormProvider sfp = kit.getShortFormProvider();

        return parserService.autocomplete(expression, df, checker, finder, sfp).toString();
    }

    // TODO return the actual ParseResult or an XML rendering of the parse exception
    @GetMapping(value = "/parse", produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String parseOWLClassExpression(
            @RequestParam String expression) {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();

        try {
            return parserService.parse(expression, df, checker).toString();
        } catch (ParseException e) {
            return e.toString();
        }
    }

    private Characteristic buildCharacteristic(
            final String name,
            final Set<OWLEntity> results,
            final Comparator<OWLObject> comp,
            final int start,
            final int pageSize) {

        OWLOntology ont = reasonerService.getReasoningActiveOnt();

        List<AxiomWithMetadata> result = results.stream()
                .sorted(comp)
                .map(e -> new AxiomWithMetadata("result", e, null, OWLUtils.getDeclaringOntology(e, ont)))
                .skip(start).limit(pageSize)
                .toList();

        PageData pageData = new PageData(start, pageSize, results.size());

        return new Characteristic(null, name, result, pageData);
    }
}
