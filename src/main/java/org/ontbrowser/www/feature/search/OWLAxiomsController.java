package org.ontbrowser.www.feature.search;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.service.OWLAxiomService;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nonnull;

import static org.ontbrowser.www.feature.search.HighlightingHTMLRenderer.getHighlightRenderer;

@RestController
@RequestMapping(value = "/axioms")
public class OWLAxiomsController extends ApplicationController {

    private static final String MODEL_KEY_AXIOMS = "axioms";
    private static final String LOGICAL_AXIOMS_TYPE = "logicalAxioms";

    private final OWLAxiomService axiomService;

    public OWLAxiomsController(
            @Autowired OWLAxiomService axiomService) {
        this.axiomService = axiomService;
    }

    @GetMapping(value = "/")
    public ModelAndView axioms(
            final Model model,
            @RequestParam(required = false) String search,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) String type,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            HttpServletRequest request
    ) {

        if (search != null) {
            validateSearch(search);
            model.addAttribute("mos", getHighlightRenderer(search, rendererFactory.getHTMLRenderer(ont)));
            ShortFormProvider sfp = kit.getShortFormProvider();
            if (type == null) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.findAxioms(search, ont, sfp, start, pageSize));
            } else if (type.equals(LOGICAL_AXIOMS_TYPE)) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.findLogicalAxioms(search, ont, sfp, start, pageSize));
            } else {
                var axiomType = getAxiomType(type);
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.findAxiomsByType(search, ont, sfp, start, pageSize, axiomType));
            }
        } else {
            model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));
            if (type == null) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.getAxioms(ont, Imports.INCLUDED, start, pageSize));
            } else if (type.equals(LOGICAL_AXIOMS_TYPE)) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.getLogicalAxioms(ont, Imports.INCLUDED, start, pageSize));
            } else {
                var axiomType = getAxiomType(type);
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.getAxiomsOfType(ont, Imports.INCLUDED, start, pageSize, axiomType));
            }
        }

        model.addAttribute("title", getTitle(search, type, ont));
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));

        return new ModelAndView("axioms");
    }

    private String getTitle(String search, String type, OWLOntology ont) {

        var sb = new StringBuilder();
        sb.append(kit.getOntologySFP().getShortForm(ont));
        sb.append(" - ");

        if (search != null) {
            sb.append("search ");
        }
        if (type == null) {
            sb.append("axioms");
        } else if (type.equals(LOGICAL_AXIOMS_TYPE)) {
            sb.append("logical axioms");
        } else {
            sb.append(type);
            sb.append(" axioms");
        }
        if (search != null) {
            sb.append(": ");
            sb.append(search);
        }

        return sb.toString();
    }

    // Prevent injection attacks
    private static void validateSearch(@Nonnull String search) {
        if (search.contains("<") || search.contains(">") || search.contains("%")) {
            throw new IllegalArgumentException("Search terms may be text only");
        }
    }

    private AxiomType<OWLAxiom> getAxiomType(String type) {
        if (type != null) {
            var axiomType = (AxiomType<OWLAxiom>) AxiomType.getAxiomType(type);
            if (axiomType == null) {
                throw new IllegalArgumentException("Unknown axiom type: " + type);
            }
            return axiomType;
        }
        return null;
    }
}
