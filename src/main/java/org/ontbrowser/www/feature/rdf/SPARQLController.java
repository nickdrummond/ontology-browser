package org.ontbrowser.www.feature.rdf;

import jakarta.servlet.http.HttpServletRequest;
import org.apache.jena.query.QueryParseException;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.PageData;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;

import static org.ontbrowser.www.feature.rdf.SPARQLService.DEFAULT_SELECT;

@Profile("rdf")
@RestController
@RequestMapping("/sparql")
public class SPARQLController {

    private static final Logger log = LoggerFactory.getLogger(SPARQLController.class);

    private final OWLHTMLKit kit;
    private final SPARQLService sparqlService;

    public SPARQLController(
            OWLHTMLKit kit,
            SPARQLService sparqlService
    ) {
        this.kit = kit;
        this.sparqlService = sparqlService;
    }

    @GetMapping
    public ModelAndView sparql() {
        return new ModelAndView("redirect:/sparql/select");
    }

    @GetMapping("/select")
    public ModelAndView select(
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
            model.addAttribute("select", DEFAULT_SELECT);
        } else {
            try {
                var results = sparqlService.select(prefixes + select + "\nLIMIT " + pageSize + " OFFSET " + (start - 1), ont);
                model.addAttribute("results", results);
            } catch (QueryParseException e) {
                model.addAttribute("error", e.getMessage());
            }

            model.addAttribute("select", select);
        }
        return new ModelAndView("sparql");
    }
}
