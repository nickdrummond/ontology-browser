package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.entities.OWLClassesService;
import org.ontbrowser.www.feature.entities.OWLIndividualsService;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
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
// TODO control over incoming direction
// TODO secondary indivs and dynamic loading
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
            @RequestParam(name = "top", required = false, defaultValue = "type") final List<String> topWithProperties,
            @RequestParam(name = "bottom", required = false, defaultValue = "") final List<String> bottomWithProperties,
            @RequestParam(name = "left", required = false, defaultValue = "") final List<String> leftWithProperties,
            @RequestParam(name = "right", required = false, defaultValue = "") final List<String> rightWithProperties,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        individualFragment(individualId, topWithProperties, bottomWithProperties, leftWithProperties, rightWithProperties, ont, model);
        return new ModelAndView("graph");
    }

    // default right
    @GetMapping(value = "/fragment/individuals/{individualId}")
    public ModelAndView individualFragment(
            @PathVariable final String individualId,
            @RequestParam(name = "top", required = false, defaultValue = "type") final List<String> topWithProperties,
            @RequestParam(name = "bottom", required = false, defaultValue = "") final List<String> bottomWithProperties,
            @RequestParam(name = "left", required = false, defaultValue = "") final List<String> leftWithProperties,
            @RequestParam(name = "right", required = false, defaultValue = "") final List<String> rightWithProperties,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        var ind = individualsService.getOWLIndividualFor(individualId, ont);
        if (ind == null) {
            throw new NotFoundException("No individual " + individualId);
        }
        return common(ind, topWithProperties, bottomWithProperties, leftWithProperties, rightWithProperties, ont, model);
    }

    // default right
    @GetMapping(value = "/classes/{classId}")
    public ModelAndView cls(
            @PathVariable final String classId,
            @RequestParam(name = "top", required = false, defaultValue = "type") final List<String> topWithProperties,
            @RequestParam(name = "bottom", required = false, defaultValue = "") final List<String> bottomWithProperties,
            @RequestParam(name = "left", required = false, defaultValue = "") final List<String> leftWithProperties,
            @RequestParam(name = "right", required = false, defaultValue = "") final List<String> rightWithProperties,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        clsFragment(classId, topWithProperties, bottomWithProperties, leftWithProperties, rightWithProperties, ont, model);
        return new ModelAndView("graph");
    }


    // default right
    @GetMapping(value = "/fragment/classes/{classId}")
    public ModelAndView clsFragment(
            @PathVariable final String classId,
            @RequestParam(name = "top", required = false, defaultValue = "type") final List<String> topWithProperties,
            @RequestParam(name = "bottom", required = false, defaultValue = "") final List<String> bottomWithProperties,
            @RequestParam(name = "left", required = false, defaultValue = "") final List<String> leftWithProperties,
            @RequestParam(name = "right", required = false, defaultValue = "") final List<String> rightWithProperties,
            @ModelAttribute final OWLOntology ont,
            Model model
    ) throws NotFoundException {
        var cls = classesService.getOWLClassFor(classId, ont);
        if (cls == null) {
            throw new NotFoundException("No class " + classId);
        }

        return common(cls, topWithProperties, bottomWithProperties, leftWithProperties, rightWithProperties, ont, model);
    }

    private ModelAndView common(
            OWLEntity entity,
            final List<String> topWithProperties,
            final List<String> bottomWithProperties,
            final List<String> leftWithProperties,
            final List<String> rightWithProperties,
            OWLOntology ont, Model model) {

        var shortForm = kit.getShortFormProvider().getShortForm(entity);
        var df = ont.getOWLOntologyManager().getOWLDataFactory();
        var zoneDescriptors = new ZoneDescriptors(topWithProperties, bottomWithProperties, leftWithProperties, rightWithProperties);
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
