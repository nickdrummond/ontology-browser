package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.renderer.ElementRenderer;
import org.ontbrowser.www.service.OWLAxiomService;
import org.ontbrowser.www.service.OWLOntologiesService;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import static org.ontbrowser.www.renderer.HighlightingHTMLRenderer.getHighlightRenderer;

@Controller
@RequestMapping(value = "/axioms")
public class OWLAxiomsController extends ApplicationController {

    @Autowired
    private OWLOntologiesService service;

    @Autowired
    private OWLAxiomService axiomService;

    @GetMapping(value = "/")
    public String axioms(
        final Model model,
        @RequestParam(required = false) Optional<String> search,
        @RequestParam(required = false, defaultValue = "true") boolean includeImports,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false, defaultValue = "1") int start,
        @RequestParam(required = false) final String ontId,
        HttpServletRequest request
    ) throws NotFoundException {

        OWLOntology ont = ontId == null ? kit.getActiveOntology() : service.getOntologyFor(ontId, kit);

        Set<OWLOntology> onts = includeImports ? ont.getImportsClosure() : Collections.singleton(ont);

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
                .map(s -> axiomService.findAxioms(search.get(), onts, kit.getShortFormProvider(), start, pageSize))
                .orElse(axiomService.getAxioms(onts, start, pageSize));

        model.addAttribute("title", "Axioms");
        model.addAttribute("axioms", axioms);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));

        return "axioms";
    }
}
