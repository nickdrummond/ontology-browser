package org.ontbrowser.www.feature.axioms;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;
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

    OWLAxiomsController(
            RestartableKit kit,
            OWLAxiomService axiomService,
            CommonContent commonContent) {
        super(kit);
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
            HttpServletRequest request
    ) throws IOException {
        return axioms(model, search, ont, imports, type, pageSize, start, request);
    }

    @GetMapping()
    public ModelAndView axioms(
            final Model model,
            @RequestParam(required = false) @Nullable String search,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "INCLUDED") Imports imports,
            @RequestParam(required = false) @Nullable String type,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start,
            HttpServletRequest request
    ) throws IOException {

        if (start < 1) {
            start = 1;
        }
        if (pageSize < 1) {
            pageSize = DEFAULT_PAGE_SIZE;
        }
        if (search != null) {
            if (search.isBlank()) {
                return new ModelAndView("redirect:/axioms");
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
        var scheme = new GlobalPagingURIScheme(request.getQueryString());

        commonContent.addCommonContent(request.getQueryString(), model, ont);
        model.addAttribute("title", getTitle(search, type, imports));
        model.addAttribute("axiomTypes", getAxiomTypes(scheme));

        return new ModelAndView("axioms");
    }

    private String getTitle(@Nullable String search, @Nullable String type, Imports imports) {

        var sb = new StringBuilder();
        if (search != null) {
            sb.append("Search: ");
        }
        if (type == null) {
            sb.append("All axioms");
        } else if (type.equals(LOGICAL_AXIOMS_TYPE)) {
            sb.append("Logical axioms");
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
}
