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
import java.util.Comparator;
import java.util.stream.Stream;

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
            @RequestParam(required = false, defaultValue = "INCLUDED") Imports imports,
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

        model.addAttribute("title", getTitle(search, type, ont, imports));
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));
        model.addAttribute("axiomTypes", getAxiomTypes());

        return new ModelAndView("axioms");
    }

    private Stream<AxiomTypeData> getAxiomTypes() {
        return AxiomType.AXIOM_TYPES.stream()
                .map(t -> new AxiomTypeData(
                        t,
                        splitCamelCase(t.getName()),
                        getCategory(t)
                ))
                .sorted(Comparator.comparing(AxiomTypeData::category).thenComparing(AxiomTypeData::name));
    }

    private static String getCategory(AxiomType<?> t) {
        if (AxiomType.TBoxAxiomTypes.contains(t))
            return "T";
        if (AxiomType.ABoxAxiomTypes.contains(t))
            return "A";
        if (AxiomType.RBoxAxiomTypes.contains(t))
            return "R";
        return "-";
    }

    // https://stackoverflow.com/questions/2559759/how-do-i-convert-camelcase-into-human-readable-names-in-java
    static String splitCamelCase(String s) {
        return s.replaceAll(
                String.format("%s|%s|%s",
                        "(?<=[A-Z])(?=[A-Z][a-z])",
                        "(?<=[^A-Z])(?=[A-Z])",
                        "(?<=[A-Za-z])(?=[^A-Za-z])"
                ),
                " "
        );
    }

    public record AxiomTypeData(
            AxiomType type,
            String name,
            String category
    ) {
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
