package org.coode.www.controller;

import org.coode.html.doclet.NodeDoclet;
import org.coode.html.doclet.OWLClassSummaryDoclet;
import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.html.doclet.HierarchyDocletFactory;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.service.OWLClassesService;

import org.coode.www.service.OWLIndividualsService;
import org.semanticweb.owlapi.model.OWLClass;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.net.URL;

@Controller
@RequestMapping(value="/classes")
@SessionAttributes("kit")
public class OWLClassesController extends ApplicationController {

    @Autowired
    private OWLClassesService service;

    @Autowired
    private OWLIndividualsService individualsService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLClasses(@RequestParam(required=false) final String label,
                                final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLClass owlThing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        String id = service.getIdFor(owlThing);

        return "redirect:/classes/" + id;
    }


    @RequestMapping(value="/{classId}", method=RequestMethod.GET)
    public String getOWLClass(@PathVariable final String classId,
                              @RequestParam(required=false) final String label,
                              final HttpServletRequest request,
                              final Model model) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLClass owlClass = service.getOWLClassFor(classId, kit);

        // TODO yuck replace this adapter
        HierarchyDocletFactory hierarchyDocletFactory = new HierarchyDocletFactory(kit);
        HTMLDoclet hierarchyDoclet = hierarchyDocletFactory.getHierarchy(OWLClass.class);
        hierarchyDoclet.setUserObject(owlClass);
        HTMLDoclet summaryDoclet = new OWLClassSummaryDoclet(kit);
        summaryDoclet.setUserObject(owlClass);

        model.addAttribute("applicationInfo", applicationInfo);
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, summaryDoclet, hierarchyDoclet));

        return "doclet";
    }

    @RequestMapping(value="/{classId}/children", method=RequestMethod.GET)
    @ResponseBody
    public String getChildren(@PathVariable final String classId,
                              @RequestParam(required=false) final String label,
                              @RequestHeader final URL referer,
                              final HttpServletRequest request) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLClass owlClass = service.getOWLClassFor(classId, kit);
        HierarchyProvider<OWLClass> hp = service.getHierarchyProvider(kit);
        NodeDoclet<OWLClass> nodeDoclet = new NodeDoclet<OWLClass>(kit, owlClass, hp);
        nodeDoclet.setUserObject(null); // not sure why wee need this, but otherwise no children

        return renderDoclets(referer, nodeDoclet);
    }

    @RequestMapping(value="/{classId}/instances", method=RequestMethod.GET)
    @ResponseBody
    public String getInstances(@PathVariable final String classId,
                               @RequestParam(required=false) final String label,
                               @RequestHeader final URL referer,
                               final HttpServletRequest request) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLClass owlClass = service.getOWLClassFor(classId, kit);
        HierarchyProvider<OWLNamedIndividual> hp = individualsService.getHierarchyProvider(kit);
        NodeDoclet nodeDoclet = new NodeDoclet(kit, owlClass, hp);
        nodeDoclet.setUserObject(null); // not sure why wee need this, but otherwise no children

        return renderDoclets(referer, nodeDoclet);
    }
}
