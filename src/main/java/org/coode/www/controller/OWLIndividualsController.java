package org.coode.www.controller;

import com.google.common.base.Optional;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.HierarchyDocletFactory;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.MediaRenderer;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.GeoService;
import org.coode.www.service.MediaService;
import org.coode.www.service.OWLIndividualsService;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Set;

@Controller
@RequestMapping(value="/individuals")
@SessionAttributes("kit")
public class OWLIndividualsController extends ApplicationController {

    @Autowired
    private OWLIndividualsService service;

    @Autowired
    private GeoService geoService;

    @Autowired
    private MediaService mediaService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLIndividuals(@RequestParam(required=false) final String label,
                                final HttpServletRequest request,
                                final Model model) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label, model);

        OWLNamedIndividual firstIndividual = service.getFirstIndividual(kit);

        String id = service.getIdFor(firstIndividual);

        return "redirect:/individuals/" + id;
    }


    @RequestMapping(value= "/{individualId}", method=RequestMethod.GET)
    public String getOWLIndividual(@PathVariable final String individualId,
                              @RequestParam(required=false) final String label,
                              final HttpServletRequest request,
                              final Model model) throws OntServerException, NotFoundException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label, model);

        OWLNamedIndividual owlIndividual = service.getOWLIndividualFor(individualId, kit);

        // TODO yuck replace this adapter
        HierarchyDocletFactory hierarchyDocletFactory = new HierarchyDocletFactory(kit);
        HTMLDoclet hierarchyDoclet = hierarchyDocletFactory.getHierarchy(OWLNamedIndividual.class);
        hierarchyDoclet.setUserObject(owlIndividual);

        String entityName = kit.getOWLServer().getShortFormProvider().getShortForm(owlIndividual);

        OWLHTMLRenderer owlRenderer = new MediaRenderer(kit, owlIndividual);

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();

        Optional<GeoService.Loc> maybeLoc = geoService.getLocation(owlIndividual, ontologies);
        if (maybeLoc.isPresent()) {
            GeoService.Loc loc = maybeLoc.get();
            model.addAttribute("geo", loc);
        }

        if (mediaService.isImageURL(owlIndividual.getIRI())) {
            model.addAttribute("image", owlIndividual.getIRI().toString());
        }

        if (mediaService.isSoundURL(owlIndividual.getIRI())) {
            model.addAttribute("sound", owlIndividual.getIRI().toString());
        }

        model.addAttribute("title", entityName + " (Individual)");
        model.addAttribute("iri", owlIndividual.getIRI().toString());
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("characteristics", service.getCharacteristics(owlIndividual, kit));
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("content", renderDoclets(request, hierarchyDoclet));

        return "owlentity";
    }
}
