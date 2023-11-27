package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.Tree;
import org.coode.www.renderer.MediaRenderer;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.*;
import org.coode.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Controller
@RequestMapping(value="/individuals")
public class OWLIndividualsController extends ApplicationController {

    @Autowired
    private OWLIndividualsService service;

    @Autowired
    private OWLOntologiesService ontService;

    @Autowired
    private GeoService geoService;

    @Autowired
    private MediaService mediaService;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLIndividuals() throws NotFoundException {

        OWLNamedIndividual firstIndividual = service.getFirstIndividual(kit);

        String id = service.getIdFor(firstIndividual);

        return "redirect:/individuals/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value= "/{individualId}", method=RequestMethod.GET)
    public String getOWLIndividual(@PathVariable final String individualId,
                                   @RequestParam(required=false) final String ontId,
                                   final HttpServletRequest request,
                                   final Model model) throws NotFoundException {

        OWLOntology activeOntology = kit.getActiveOntology();

        if (ontId != null) {
            activeOntology = ontService.getOntologyFor(ontId, kit);
        }

        Set<OWLOntology> ontologies = activeOntology.getImportsClosure();

        OWLNamedIndividual owlIndividual = service.getOWLIndividualFor(individualId, ontologies);

        Comparator<Tree<OWLEntity>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(activeOntology);

        OWLIndividualsByTypeHierarchyService hierarchyService = new OWLIndividualsByTypeHierarchyService(r, comparator);

        Tree<OWLEntity> prunedTree = hierarchyService.getPrunedTree(owlIndividual);

        ShortFormProvider sfp = kit.getShortFormProvider();

        String entityName = sfp.getShortForm(owlIndividual);

        OWLHTMLRenderer owlRenderer = new MediaRenderer(kit).withActiveObject(owlIndividual);

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

        List<Characteristic> characteristics = service.getCharacteristics(owlIndividual, ontologies, kit.getComparator());

        model.addAttribute("title", entityName + " (Individual)");
        model.addAttribute("type", "Individuals");
        model.addAttribute("iri", owlIndividual.getIRI());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }
}
