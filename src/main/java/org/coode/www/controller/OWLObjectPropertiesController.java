package org.coode.www.controller;

import org.coode.html.doclet.NodeDoclet;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.html.page.SummaryPageFactory;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.www.ServletUtils;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.service.OWLObjectPropertiesService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

@Controller
@RequestMapping(value="/objectproperties")
@SessionAttributes("kit")
public class OWLObjectPropertiesController extends ApplicationController {

    @Autowired
    private OWLObjectPropertiesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLObjectProperties(@RequestParam(required=false) final String label,
                                final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();

        String id = service.getIdFor(owlTopObjectProperty);

        return "redirect:/objectproperties/" + id;
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLObjectProperty(@PathVariable final String propertyId,
                              @RequestParam(required=false) final String label,
                              final HttpServletRequest request,
                              Model model) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLObjectProperty owlObjectProperty = service.getOWLObjectPropertyFor(propertyId, kit);

        // TODO yuck replace this adapter
        SummaryPageFactory summaryPageFactory = new SummaryPageFactory(kit);
        HTMLDoclet hierarchyDoclet = summaryPageFactory.getHierarchy(OWLObjectProperty.class);
        hierarchyDoclet.setUserObject(owlObjectProperty);
        HTMLDoclet summaryDoclet = summaryPageFactory.getSummaryDoclet(owlObjectProperty);

        model.addAttribute("application", applicationInfo);
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

        OWLObjectProperty property = service.getOWLObjectPropertyFor(propertyId, kit);
        HierarchyProvider<OWLObjectProperty> hp = service.getHierarchyProvider(kit);
        NodeDoclet<OWLObjectProperty> nodeDoclet = new NodeDoclet<OWLObjectProperty>(kit, property, hp);
        nodeDoclet.setUserObject(null); // not sure why wee need this, but otherwise no children

        return renderDoclets(referer, nodeDoclet);
    }
}
