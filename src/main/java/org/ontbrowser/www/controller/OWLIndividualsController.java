package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.*;
import org.ontbrowser.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

@RestController
@RequestMapping(value="/individuals")
public class OWLIndividualsController extends ApplicationController {

    private final OWLIndividualsService service;
    private final MediaService mediaService;
    private final ReasonerFactoryService reasonerFactoryService;
    private final ReasonerService reasonerService;

    public OWLIndividualsController(
            @Autowired OWLIndividualsService service,
            @Autowired MediaService mediaService,
            @Autowired ReasonerFactoryService reasonerFactoryService,
            @Autowired ReasonerService reasonerService) {
        this.service = service;
        this.mediaService = mediaService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.reasonerService = reasonerService;
    }

    @GetMapping(value="/")
    public void getOWLIndividuals(
            final HttpServletResponse response
    ) throws IOException, NotFoundException {

        OWLNamedIndividual firstIndividual = service.getFirstIndividual(kit);

        String id = service.getIdFor(firstIndividual);

        response.sendRedirect("/individuals/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value= "/{individualId}")
    public ModelAndView getOWLIndividual(
        @PathVariable final String individualId,
        @RequestParam(defaultValue = "false") final boolean inferred,
        @RequestParam(required = false) List<With> with,
        @ModelAttribute final OWLOntology ont,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLNamedIndividual owlIndividual = service.getOWLIndividualFor(individualId, ont);

        Comparator<Tree<OWLEntity>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLIndividualsByTypeHierarchyService hierarchyService = new OWLIndividualsByTypeHierarchyService(r, comparator);

        Tree<OWLEntity> prunedTree = hierarchyService.getPrunedTree(owlIndividual);

        model.addAttribute("hierarchy", prunedTree);

        getOWLIndividualFragment(individualId, inferred, with, ont, model, request, response);

        return new ModelAndView("owlentity");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value= "/{individualId}/fragment")
    public ModelAndView getOWLIndividualFragment(
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inferred,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLNamedIndividual owlIndividual = service.getOWLIndividualFor(individualId, ont);

        ShortFormProvider sfp = kit.getShortFormProvider();

        String entityName = sfp.getShortForm(owlIndividual);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlIndividual);

        if (mediaService.isImageURL(owlIndividual.getIRI())) {
            model.addAttribute("image", owlIndividual.getIRI().toString());
        }

        if (mediaService.isSoundURL(owlIndividual.getIRI())) {
            model.addAttribute("sound", owlIndividual.getIRI().toString());
        }

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = new ArrayList<>(
                service.getCharacteristics(owlIndividual, ont, kit.getComparator(), withOrEmpty, DEFAULT_PAGE_SIZE));

        if (inferred) {
            OWLReasoner reasoner = reasonerService.getReasoner();
            characteristics.addAll(service.getInferredCharacteristics(owlIndividual, reasoner));
        }

        Set<OWLClass> namedTypes = service.getNamedTypes(owlIndividual, ont);

        String types = String.join(", ", namedTypes.stream().map(sfp::getShortForm).toList());

        String title = entityName + (types.isEmpty() ? "" : " (" + types + ")");

        model.addAttribute("title", title);
        model.addAttribute("type", "Individuals");
        model.addAttribute("iri", owlIndividual.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));
        model.addAttribute("mos", owlRenderer);

        response.addHeader("title", projectInfo.getName() + ": " + title);

        return new ModelAndView("owlentityfragment");

    }
}
