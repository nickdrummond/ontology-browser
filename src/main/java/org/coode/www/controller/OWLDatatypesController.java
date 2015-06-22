package org.coode.www.controller;

import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.HierarchyDocletFactory;
import org.coode.html.doclet.NodeDoclet;
import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLDatatypesService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.net.URL;

@Controller
@RequestMapping(value="/datatypes")
@SessionAttributes("kit")
public class OWLDatatypesController extends ApplicationController {

    @Autowired
    private OWLDatatypesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLDatatypes(@ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        OWLDatatype owlTopDatatype = df.getTopDatatype();

        String id = service.getIdFor(owlTopDatatype);

        return "redirect:/datatypes/" + id;
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLDatatype(@PathVariable final String propertyId,
                                 @ModelAttribute("kit") final OWLHTMLKit kit,
                                 final HttpServletRequest request,
                                 final Model model) throws OntServerException, NotFoundException {

        OWLDatatype owlDatatype = service.getOWLDatatypeFor(propertyId, kit);

        // TODO yuck replace this adapter
        HierarchyDocletFactory hierarchyDocletFactory = new HierarchyDocletFactory(kit);
        HTMLDoclet hierarchyDoclet = hierarchyDocletFactory.getHierarchy(OWLDatatype.class);
        hierarchyDoclet.setUserObject(owlDatatype);

        String entityName = kit.getOWLServer().getShortFormProvider().getShortForm(owlDatatype);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, owlDatatype);

        model.addAttribute("title", entityName + " (Datatype)");
        model.addAttribute("iri", owlDatatype.getIRI().toString());
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("characteristics", service.getCharacteristics(owlDatatype, kit));
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("content", renderDoclets(request, hierarchyDoclet));

        return "owlentity";
    }

    @ResponseBody
    @RequestMapping(value="/{datatypeId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String datatypeId,
                              @ModelAttribute("kit") final OWLHTMLKit kit,
                              @RequestHeader final URL referer) throws OntServerException, NotFoundException {

        OWLDatatype datatype = service.getOWLDatatypeFor(datatypeId, kit);
        HierarchyProvider<OWLDatatype> hp = service.getHierarchyProvider(kit);
        NodeDoclet<OWLDatatype> nodeDoclet = new NodeDoclet<OWLDatatype>(kit, datatype, hp);
        nodeDoclet.setUserObject(null); // not sure why wee need this, but otherwise no children

        return renderDoclets(referer, nodeDoclet);
    }
}
