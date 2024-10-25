package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.renderer.ElementRenderer;
import org.ontbrowser.www.service.OWLAxiomService;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletRequest;
import java.util.Optional;

import static org.ontbrowser.www.feature.search.HighlightingHTMLRenderer.getHighlightRenderer;

@RestController
@RequestMapping(value = "/axioms")
public class OWLAxiomsController extends ApplicationController {

    private final OWLAxiomService axiomService;

    public OWLAxiomsController(
            @Autowired OWLAxiomService axiomService) {
        this.axiomService = axiomService;
    }

    @GetMapping(value = "/")
    public ModelAndView axioms(
        final Model model,
        @RequestParam(required = false) Optional<String> search,
        @ModelAttribute final OWLOntology ont,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false, defaultValue = "1") int start,
        HttpServletRequest request
    ) {

        search.ifPresent(s -> {
            // Prevent injection attacks
            if (s.contains("<") || s.contains(">") || s.contains("%")) {
                throw new IllegalArgumentException("Search terms may be text only");
            }
        });

        ElementRenderer<OWLObject> owlRenderer = search
                .map(s -> getHighlightRenderer(s, rendererFactory.getRenderer(ont)))
                .orElse(rendererFactory.getRenderer(ont));

        Characteristic axioms = search
                .map(s -> axiomService.findAxioms(search.get(), ont, kit.getShortFormProvider(), start, pageSize))
                .orElse(axiomService.getAxioms(ont, Imports.INCLUDED, start, pageSize));

        model.addAttribute("title", "Axioms");
        model.addAttribute("axioms", axioms);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));

        return new ModelAndView("axioms");
    }
}
