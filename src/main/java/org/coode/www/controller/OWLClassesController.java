package org.coode.www.controller;

import org.coode.html.OWLHTMLKit;
import org.coode.html.SummaryPageFactory;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.www.ServletUtils;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.service.OWLClassesService;

import org.semanticweb.owlapi.model.OWLClass;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

@Controller
@RequestMapping(value="/classes")
@SessionAttributes("kit")
public class OWLClassesController extends ApplicationController {

    @Autowired
    private OWLClassesService owlClassesService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOntology(@RequestParam(required=false) final String label,
                              final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLClass owlThing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        String id = owlClassesService.getIdFor(owlThing);

        return "redirect:/classes/" + id;
    }


    @RequestMapping(value="/{classId}", method=RequestMethod.GET)
    public String getOntology(@PathVariable final String classId,
                              @RequestParam(required=false) final String label,
                              final HttpServletRequest request,
                              Model model) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLClass owlClass = owlClassesService.getOWLClassFor(classId, kit);

        // TODO yuck replace this adapter
        SummaryPageFactory summaryPageFactory = new SummaryPageFactory(kit);
        HTMLDoclet hierarchyDoclet = summaryPageFactory.getHierarchy(OWLClass.class);
        hierarchyDoclet.setUserObject(owlClass);
        HTMLDoclet summaryDoclet = summaryPageFactory.getSummaryDoclet(owlClass);

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
