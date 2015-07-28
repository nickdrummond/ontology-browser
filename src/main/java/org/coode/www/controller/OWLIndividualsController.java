package org.coode.www.controller;

import com.google.common.base.Optional;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.renderer.MediaRenderer;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.GeoService;
import org.coode.www.service.MediaService;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;
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
    public String getOWLIndividuals(@ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException, NotFoundException {

        OWLNamedIndividual firstIndividual = service.getFirstIndividual(kit);

        String id = service.getIdFor(firstIndividual);

        return "redirect:/individuals/" + id;
    }


    @RequestMapping(value= "/{individualId}", method=RequestMethod.GET)
    public String getOWLIndividual(@PathVariable final String individualId,
                                   @ModelAttribute("kit") final OWLHTMLKit kit,
                                   final HttpServletRequest request,
                                   final Model model) throws OntServerException, NotFoundException {

        OWLNamedIndividual owlIndividual = service.getOWLIndividualFor(individualId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLEntity>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLIndividualsByTypeHierarchyService hierarchyService =
                new OWLIndividualsByTypeHierarchyService(owlServer.getOWLReasoner(), comparator);

        Tree<OWLEntity> prunedTree = hierarchyService.getPrunedTree(owlIndividual);

        String entityName = kit.getOWLServer().getShortFormProvider().getShortForm(owlIndividual);

        OWLHTMLRenderer owlRenderer = new MediaRenderer(kit, Optional.of(owlIndividual));

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
        model.addAttribute("type", "Individuals");
        model.addAttribute("iri", owlIndividual.getIRI());
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", service.getCharacteristics(owlIndividual, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }
}
