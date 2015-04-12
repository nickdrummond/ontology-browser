package org.coode.www.controller;

import org.coode.html.doclet.NodeDoclet;
import org.coode.html.doclet.OWLAnnotationPropertySummaryDoclet;
import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.html.doclet.HierarchyDocletFactory;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
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
        HierarchyDocletFactory hierarchyDocletFactory = new HierarchyDocletFactory(kit);
        HTMLDoclet hierarchyDoclet = hierarchyDocletFactory.getHierarchy(OWLAnnotationProperty.class);
        hierarchyDoclet.setUserObject(owlAnnotationProperty);
        HTMLDoclet summaryDoclet = new OWLAnnotationPropertySummaryDoclet(kit);
        summaryDoclet.setUserObject(owlAnnotationProperty);

        model.addAttribute("applicationInfo", applicationInfo);
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, summaryDoclet, hierarchyDoclet));

        return "doclet";
    }

    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    @ResponseBody
    public String getChildren(@PathVariable final String propertyId,
                              @RequestParam(required=false) final String label,
                              @RequestHeader final URL referer,
                              final HttpServletRequest request) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLAnnotationProperty property = service.getOWLAnnotationPropertyFor(propertyId, kit);
        HierarchyProvider<OWLAnnotationProperty> hp = service.getHierarchyProvider(kit);
        NodeDoclet<OWLAnnotationProperty> nodeDoclet = new NodeDoclet<OWLAnnotationProperty>(kit, property, hp);
        nodeDoclet.setUserObject(null); // not sure why wee need this, but otherwise no children

        return renderDoclets(referer, nodeDoclet);
    }
}
