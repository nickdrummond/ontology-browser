package org.coode.www.controller;

import org.coode.html.url.URLScheme;
import org.coode.www.cloud.IndividualsByUsageCloud;
import org.coode.www.model.XmlUrl;
import org.coode.www.model.XmlUrlSet;
import org.coode.www.renderer.ElementRenderer;
import org.coode.www.renderer.Highlighter;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.CloudHelper;
import org.coode.www.service.OWLAxiomService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.servlet.http.HttpServletRequest;
import java.util.Set;
import java.util.function.Consumer;

@Controller
@RequestMapping(value="/axioms")
public class OWLAxiomsController extends ApplicationController {

    @Autowired
    private OWLAxiomService axiomService;

    @RequestMapping(value = "/")
    public String axioms(final Model model,
                         @RequestParam(required = false) String search,
                         @RequestParam(required = false) String regex) {
        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

        ElementRenderer<OWLObject> highlightRenderer = new ElementRenderer<OWLObject>() {

            private Highlighter highlighter = new Highlighter(search);

            @Override
            public String render(OWLObject object) {
                return highlighter.highlight(owlRenderer.render(object));
            }
        };

        Set<OWLAxiom> axioms = (regex != null) ?
                axiomService.regexAxioms(regex, kit.getActiveOntology(), kit.getShortFormProvider()) :
                axiomService.findAxioms(search, kit.getActiveOntology(), kit.getShortFormProvider());

        model.addAttribute("title", axioms.size() + " axioms containing: " + search);
        model.addAttribute("axioms", axioms);
        model.addAttribute("mos", highlightRenderer);

        return "axioms";
    }
}
