package org.ontbrowser.www.feature.dlquery;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.exception.OntServerException;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.graph.GraphURLScheme;
import org.ontbrowser.www.feature.parser.ParserService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.PageData;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.ontbrowser.www.util.OWLUtils;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static org.ontbrowser.www.controller.Constants.DEFAULT_PAGE_SIZE_STR;

@RestController
@RequestMapping(value = DLQueryController.PATH)
public class DLQueryController {

    private static final Logger log = LoggerFactory.getLogger(DLQueryController.class);
    public static final String PATH = "/dlquery";

    private final OWLHTMLKit kit;
    private final ParserService parserService;
    private final ReasonerService reasonerService;
    private final boolean graphEnabled;

    public DLQueryController(
            OWLHTMLKit kit,
            ParserService parserService,
            ReasonerService reasonerService,
            Environment environment) {
        this.kit = kit;
        this.parserService = parserService;
        this.reasonerService = reasonerService;
        this.graphEnabled = Arrays.asList(environment.getActiveProfiles()).contains("graph");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping
    public ModelAndView dlQuery(
            @RequestParam(required = false, defaultValue = "") final String expression,
            @RequestParam(required = false, defaultValue = "") final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(required = false, defaultValue = "instances", name = "query") final QueryType queryType,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            final HttpServletRequest request,
            final Model model
    ) {
        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var checker = kit.getOWLEntityChecker();

        OWLClassExpression owlClassExpression = null;
        if (!expression.isEmpty()) {
            owlClassExpression = parserService.getOWLClassExpression(expression, df, checker);
            reasonerService.asyncQuery(new DLQuery(owlClassExpression, queryType));
        }

        if (!minus.isEmpty()) {
            reasonerService.asyncQuery(new DLQuery(parserService.getOWLClassExpression(minus, df, checker), queryType));
        }

        model.addAttribute("reasonerName", reasonerService.getReasoner().getReasonerName());
        model.addAttribute("reasoningOntology", reasonerService.getReasoningActiveOnt());
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("expression", expression);
        model.addAttribute("minus", minus);
        model.addAttribute("order", order);
        model.addAttribute("query", queryType);
        model.addAttribute("queries", QueryType.values());
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request.getQueryString()));

        if (graphEnabled && owlClassExpression != null) {
            var mos = new MOSStringRenderer(kit.getFinder(), ont);
            model.addAttribute("graphLink", new GraphURLScheme(mos).getURLForOWLObject(owlClassExpression, ont));
        }

        return new ModelAndView("dlquery");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "results")
    public ModelAndView getResults(
            @RequestParam final String expression,
            @RequestParam(required = false) final String minus,
            @RequestParam(required = false) final String order,
            @RequestParam(name = "query") final QueryType queryType,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            final HttpServletRequest request,
            final Model model
    ) throws OntServerException, QueryTimeoutException, ParserException {

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

            // Target links to parent page for fragment
            UriComponentsBuilder uriBuilder = ServletUriComponentsBuilder.fromCurrentRequest()
                    .replacePath(PATH)
                    .replaceQueryParam("expression", expression); // as it gets double encoded otherwise

            model.addAttribute("urlBuilder", uriBuilder);
            model.addAttribute("results", resultsCharacteristic);
            model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request.getQueryString()));

            return new ModelAndView("base::results");
        } catch (ExecutionException e) {
            throw new OntServerException(e);
        } catch (InterruptedException | TimeoutException e) {
            throw new QueryTimeoutException();
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
                .skip(start - 1L).limit(pageSize)
                .toList();

        PageData pageData = new PageData(start, pageSize, results.size());

        return new Characteristic(null, name, result, pageData);
    }
}
