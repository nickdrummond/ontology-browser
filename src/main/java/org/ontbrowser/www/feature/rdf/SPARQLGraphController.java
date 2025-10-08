package org.ontbrowser.www.feature.rdf;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.feature.graph.CytoscapeGraph;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.http.MediaType;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;
import java.util.Set;

import static org.ontbrowser.www.feature.rdf.SPARQLService.defaultQueries;

@Profile("rdf & graph")
@RestController
@RequestMapping("/sparql/graph")
public class SPARQLGraphController {

    private static final Logger log = LoggerFactory.getLogger(SPARQLGraphController.class);

    private final OWLHTMLKit kit;
    private final SPARQLService sparqlService;

    public SPARQLGraphController(
            OWLHTMLKit kit,
            SPARQLService sparqlService
    ) {
        this.kit = kit;
        this.sparqlService = sparqlService;
    }

    @GetMapping("/{queryType}")
    public ModelAndView sparqlGraph(
            @PathVariable SPARQLService.QueryType queryType,
            @Nullable @RequestParam(required = false) String prefixes,
            @Nullable @RequestParam(required = false) String select,
            @ModelAttribute final OWLOntology ont,
            final HttpServletRequest request,
            final Model model
    ) {
        if (prefixes == null) {
            prefixes = sparqlService.getDefaultPrefixes(kit);
        }

        model.addAttribute("prefixes", prefixes);

        // paging
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request.getQueryString()));

        model.addAttribute("queryType", queryType);

        model.addAttribute("select", select != null ? select : defaultQueries.get(queryType));

        return new ModelAndView("sparql-graph");
    }

    @GetMapping(value = "/{queryType}/data", produces = MediaType.APPLICATION_JSON_VALUE)
    public CytoscapeGraph graph(
            @PathVariable SPARQLService.QueryType queryType,
            @Nullable @RequestParam(required = false) String prefixes,
            @Nullable @RequestParam(required = false) String select,
            @ModelAttribute final OWLOntology ont
    ) {
        var df = ont.getOWLOntologyManager().getOWLDataFactory();
        var jenaModel = sparqlService.query(queryType, prefixes + select);

        log.info("construct graph with {} triples", jenaModel.size());

        var graph = new JenaGraphBuilder(jenaModel, df).build();

        var renderer = new MOSStringRenderer(kit.getFinder(), ont);
        return new CytoscapeGraph(graph, df, renderer, Set.of(), Set.of());
    }
}
