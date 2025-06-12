package org.ontbrowser.www.feature.axioms;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.controller.CommonContent;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;

import static org.ontbrowser.www.feature.axioms.HighlightingHTMLRenderer.getHighlightRenderer;
import static org.ontbrowser.www.feature.axioms.OWLAxiomsUtils.LOGICAL_AXIOMS_TYPE;
import static org.ontbrowser.www.feature.axioms.OWLAxiomsUtils.getAxiomTypes;
import static org.semanticweb.owlapi.model.AxiomType.getAxiomType;

// TODO remove ApplicationController
@RestController
@RequestMapping(value = "/axioms")
public class OWLAxiomsController extends ApplicationController {

    private static final String MODEL_KEY_AXIOMS = "axioms";

    private final OWLAxiomService axiomService;
    private final CommonContent commonContent;

    OWLAxiomsController(OWLAxiomService axiomService,
                        CommonContent commonContent) {
        this.axiomService = axiomService;
        this.commonContent = commonContent;
    }

    @GetMapping("/")
    public ModelAndView axiomsOld(
            final Model model,
            @RequestParam(required = false) String search,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "INCLUDED") Imports imports,
            @RequestParam(required = false) String type,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            HttpServletRequest request,
            HttpServletResponse response
    ) throws IOException {
        return axioms(model, search, ont, imports, type, pageSize, start, request, response);
    }

    @GetMapping()
    public ModelAndView axioms(
            final Model model,
            @RequestParam(required = false) String search,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "INCLUDED") Imports imports,
            @RequestParam(required = false) String type,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            HttpServletRequest request,
            HttpServletResponse response
    ) throws IOException {

        if (start < 1) {
            start = 1;
        }
        if (pageSize < 1) {
            pageSize = DEFAULT_PAGE_SIZE;
        }
        if (search != null) {
            if (search.isEmpty()) {
                response.sendRedirect("/axioms");
                return null;
            }

            model.addAttribute("mos", getHighlightRenderer(search, rendererFactory.getHTMLRenderer(ont)));
            ShortFormProvider sfp = kit.getShortFormProvider();
            if (type == null) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.findAxioms(search, ont, imports, sfp, start, pageSize));
            } else if (type.equals(LOGICAL_AXIOMS_TYPE)) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.findLogicalAxioms(search, ont, imports, sfp, start, pageSize));
            } else {
                var axiomType = getAxiomType(type);
                model.addAttribute("type", axiomType);
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.findAxiomsByType(search, ont, imports, sfp, start, pageSize, axiomType));
            }
        } else {
            model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));
            if (type == null) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.getAxioms(ont, imports, start, pageSize));
            } else if (type.equals(LOGICAL_AXIOMS_TYPE)) {
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.getLogicalAxioms(ont, imports, start, pageSize));
            } else {
                var axiomType = getAxiomType(type);
                model.addAttribute("type", axiomType);
                model.addAttribute(MODEL_KEY_AXIOMS, axiomService.getAxiomsOfType(ont, imports, start, pageSize, axiomType));
            }
        }

        commonContent.addCommonContent(request, model, ont);
        model.addAttribute("title", getTitle(search, type, ont, imports));
        model.addAttribute("axiomTypes", getAxiomTypes());

        return new ModelAndView("axioms");
    }

    private String getTitle(String search, String type, OWLOntology ont, Imports imports) {

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
        sb.append(" - imports ");
        sb.append(imports.name().toLowerCase());

        if (search != null) {
            sb.append(": ");
            sb.append(search);
        }

        return sb.toString();
    }
}
