package org.ontbrowser.www.feature.rdf;

import jakarta.servlet.http.HttpServletRequest;
import org.apache.jena.query.QueryParseException;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.model.paging.PageData;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.context.annotation.Profile;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;

import static org.ontbrowser.www.feature.rdf.SPARQLService.DEFAULT_QUERY;

@Profile("rdf")
@RestController
@RequestMapping("/sparql")
public class SPARQLController extends ApplicationController {

    private final SPARQLService sparqlService;

    public SPARQLController(SPARQLService sparqlService) {
        this.sparqlService = sparqlService;
    }

    @GetMapping
    public ModelAndView sparql(
            @Nullable @RequestParam(required = false) String prefixes,
            @Nullable @RequestParam(required = false) String select,
            @RequestParam(required = false, defaultValue = "20") int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
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
        model.addAttribute("pageData", new PageData(start, pageSize));

        if (select == null) {
            model.addAttribute("select", DEFAULT_QUERY);
        }
        else {
            try {
                var results = sparqlService.select(prefixes + select + "\nLIMIT " + pageSize + " OFFSET " + (start-1), ont);
                model.addAttribute("results", results);
                OWLHTMLRenderer htmlRenderer = rendererFactory.getHTMLRenderer(ont);
                model.addAttribute("mos", htmlRenderer);
            }
            catch (QueryParseException e) {
                model.addAttribute("error", e.getMessage());
            }

            model.addAttribute("select", select);
        }
        return new ModelAndView("sparql");
    }
}
