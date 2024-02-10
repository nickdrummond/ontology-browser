package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.Tree;
import org.coode.www.renderer.MediaRenderer;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.renderer.RendererFactory;
import org.coode.www.service.*;
import org.coode.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

    @Autowired
    private ReasonerService reasonerService;

    @GetMapping(value="/")
    public String getOWLIndividuals() throws NotFoundException {

        OWLNamedIndividual firstIndividual = service.getFirstIndividual(kit);

        String id = service.getIdFor(firstIndividual);

        return "redirect:/individuals/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value= "/{individualId}")
    public String getOWLIndividual(@PathVariable final String individualId,
                                   @RequestParam(required=false) final String ontId,
                                   @RequestParam(required=false) final boolean inferred,
                                   final Model model) throws NotFoundException {

        final OWLOntology ont = (ontId != null) ?
                ontService.getOntologyFor(ontId, kit) :
                kit.getActiveOntology();

        OWLNamedIndividual owlIndividual = service.getOWLIndividualFor(individualId, ont);

        Comparator<Tree<OWLEntity>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLIndividualsByTypeHierarchyService hierarchyService = new OWLIndividualsByTypeHierarchyService(r, comparator);

        Tree<OWLEntity> prunedTree = hierarchyService.getPrunedTree(owlIndividual);

        ShortFormProvider sfp = kit.getShortFormProvider();

        String entityName = sfp.getShortForm(owlIndividual);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlIndividual);

        Optional<GeoService.Loc> maybeLoc = geoService.getLocation(owlIndividual, ont);
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

        List<Characteristic> characteristics = service.getCharacteristics(owlIndividual, ont, kit.getComparator());

        if (inferred) {
            characteristics.addAll(service.getInferredCharacteristics(
                    owlIndividual, ont, kit.getOWLOntologyManager(), reasonerService.getReasoner()));
        }

        model.addAttribute("title", entityName + " (Individual)");
        model.addAttribute("type", "Individuals");
        model.addAttribute("iri", owlIndividual.getIRI());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }
}
