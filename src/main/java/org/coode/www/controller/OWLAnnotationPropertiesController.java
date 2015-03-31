package org.coode.www.controller;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.html.page.SummaryPageFactory;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.www.ServletUtils;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

@Controller
@RequestMapping(value="/annotationproperties")
@SessionAttributes("kit")
public class OWLAnnotationPropertiesController extends ApplicationController {

    @Autowired
    private OWLAnnotationPropertiesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLAnnotationProperties(@RequestParam(required=false) final String label,
                                final HttpServletRequest request) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLAnnotationProperty firstAnnotationProperty = service.getFirstAnnotationProperty(kit);

        String id = service.getIdFor(firstAnnotationProperty);

        return "redirect:/annotationproperties/" + id;
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLAnnotationProperty(@PathVariable final String propertyId,
                              @RequestParam(required=false) final String label,
                              final HttpServletRequest request,
                              Model model) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLAnnotationProperty owlAnnotationProperty = service.getOWLAnnotationPropertyFor(propertyId, kit);

        // TODO yuck replace this adapter
        SummaryPageFactory summaryPageFactory = new SummaryPageFactory(kit);
        HTMLDoclet hierarchyDoclet = summaryPageFactory.getHierarchy(OWLAnnotationProperty.class);
        hierarchyDoclet.setUserObject(owlAnnotationProperty);
        HTMLDoclet summaryDoclet = summaryPageFactory.getSummaryDoclet(owlAnnotationProperty);

        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);
        URL pageUrl = ServletUtils.getPageURL(request);
        hierarchyDoclet.renderAll(pageUrl, writer);
        summaryDoclet.renderAll(pageUrl, writer);
        String content = stringWriter.toString();

        model.addAttribute("application", applicationInfo);
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", content);

        return "doclet";
    }
}
