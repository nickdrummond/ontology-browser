package org.ontbrowser.www.rdf;

import org.apache.jena.query.QueryParseException;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.context.annotation.Profile;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import static org.ontbrowser.www.rdf.SPARQLService.DEFAULT_QUERY;

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
            @RequestParam(required = false) String prefixes,
            @RequestParam(required = false) String select,
            @RequestParam(required = false, defaultValue = "20") int limit,
            @ModelAttribute final OWLOntology ont,
            final Model model
    ) {
        if (prefixes == null) {
            prefixes = sparqlService.getDefaultPrefixes(kit);
        }

        model.addAttribute("prefixes", prefixes);
        model.addAttribute("limit", limit);

        if (select == null) {
            model.addAttribute("select", DEFAULT_QUERY);
        }
        else {
            try {
                var results = sparqlService.select(prefixes + select + "\nLIMIT " + limit, ont);
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
