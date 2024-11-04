package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.entities.OWLClassesService;
import org.ontbrowser.www.feature.entities.OWLIndividualsService;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.util.List;

// TODO darkmode
// TODO scrolling in pane?
// TODO nav/links from other pages
// TODO controls for up/down/left/right
// TODO navigation with keyboard?
@RestController
@Profile("graph")
@RequestMapping(value = "/graph")
public class GraphController extends ApplicationController {

    private final GraphService graphService;
    private OWLIndividualsService individualsService;
    private final OWLClassesService classesService;

    public GraphController(
            @Autowired GraphService graphService,
            @Autowired OWLIndividualsService individualsService,
            @Autowired OWLClassesService classesService
            ) {
        this.graphService = graphService;
        this.individualsService = individualsService;
        this.classesService = classesService;
    }


    // default right
    @GetMapping(value = "/individuals/{individualId}")
    public ModelAndView individual(
            @PathVariable final String individualId,
            @RequestParam(required = false, defaultValue = "") final List<String> top,
            @RequestParam(required = false, defaultValue = "") final List<String> bottom,
            @RequestParam(required = false, defaultValue = "") final List<String> left,
            @RequestParam(required = false, defaultValue = "") final List<String> right,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        individualFragment(individualId, top, bottom, left, right, ont, model);
        return new ModelAndView("graph");
    }

    @GetMapping(value = "/fragment/individuals/{individualId}")
    public ModelAndView individualFragment(
            @PathVariable final String individualId,
            @RequestParam(required = false, defaultValue = "") final List<String> top,
            @RequestParam(required = false, defaultValue = "") final List<String> bottom,
            @RequestParam(required = false, defaultValue = "") final List<String> left,
            @RequestParam(required = false, defaultValue = "") final List<String> right,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        var ind = individualsService.getOWLIndividualFor(individualId, ont);
        if (ind == null) {
            throw new NotFoundException("No individual " + individualId);
        }
        return common(ind, top, bottom, left, right, ont, model);
    }


    @GetMapping(value = "/fragment")
    public ModelAndView fragmentByIRI(
            @RequestParam final String iri,
            @RequestParam(required = false, defaultValue = "") final List<String> top,
            @RequestParam(required = false, defaultValue = "") final List<String> bottom,
            @RequestParam(required = false, defaultValue = "") final List<String> left,
            @RequestParam(required = false, defaultValue = "") final List<String> right,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        var entity = getEntityForIRI(IRI.create(iri), ont);
        return common(entity, top, bottom, left, right, ont, model);
    }

    private OWLEntity getEntityForIRI(IRI iri, OWLOntology ont) throws NotFoundException {
        if (ont.containsIndividualInSignature(iri, Imports.INCLUDED)) {
            return ont.getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(iri);
        }
        if (ont.containsClassInSignature(iri, Imports.INCLUDED)) {
            return ont.getOWLOntologyManager().getOWLDataFactory().getOWLClass(iri);
        }
        throw new NotFoundException("Cannot find class or individual with IRI " + iri);
    }

    // default right
    @GetMapping(value = "/classes/{classId}")
    public ModelAndView cls(
            @PathVariable final String classId,
            @RequestParam(required = false, defaultValue = "") final List<String> top,
            @RequestParam(required = false, defaultValue = "") final List<String> bottom,
            @RequestParam(required = false, defaultValue = "") final List<String> left,
            @RequestParam(required = false, defaultValue = "") final List<String> right,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        clsFragment(classId, top, bottom, left, right, ont, model);
        return new ModelAndView("graph");
    }

    // default right
    @GetMapping(value = "/fragment/classes/{classId}")
    public ModelAndView clsFragment(
            @PathVariable final String classId,
            @RequestParam(required = false, defaultValue = "") final List<String> top,
            @RequestParam(required = false, defaultValue = "") final List<String> bottom,
            @RequestParam(required = false, defaultValue = "") final List<String> left,
            @RequestParam(required = false, defaultValue = "") final List<String> right,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        var cls = classesService.getOWLClassFor(classId, ont);
        if (cls == null) {
            throw new NotFoundException("No class " + classId);
        }

        return common(cls, top, bottom, left, right, ont, model);
    }

    private ModelAndView common(
            OWLEntity entity,
            final List<String> top,
            final List<String> bottom,
            final List<String> left,
            final List<String> right,
            OWLOntology ont, Model model) {

        var shortForm = kit.getShortFormProvider().getShortForm(entity);
        var df = ont.getOWLOntologyManager().getOWLDataFactory();
        var zoneDescriptors = new ZoneDescriptors(top, bottom, left, right);
        var proxyBuilder = new ProxyBuilder(df);
        var zones = graphService.createZones(entity, zoneDescriptors, ont, kit.getFinder(), proxyBuilder);

        model.addAttribute("title", shortForm + " (graph)");
        model.addAttribute("subject", entity);
        model.addAttribute("proxies", proxyBuilder);
        model.addAttribute("zones", zones);
        model.addAttribute("mos", rendererFactory.getRenderer(ont).withURLScheme(new GraphURLScheme()));
        model.addAttribute("sfp", kit.getShortFormProvider());

        return new ModelAndView("graphfragment :: startnode");
    }
}
