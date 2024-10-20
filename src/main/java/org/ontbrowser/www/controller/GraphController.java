package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.model.graph.GraphBuilder;
import org.ontbrowser.www.model.graph.ProxyBuilder;
import org.ontbrowser.www.service.OWLClassesService;
import org.ontbrowser.www.service.OWLIndividualsService;
import org.ontbrowser.www.url.GraphURLScheme;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

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

    private OWLIndividualsService individualsService;
    private final OWLClassesService classesService;

    public GraphController(
            @Autowired OWLIndividualsService individualsService,
            @Autowired OWLClassesService classesService
            ) {
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
        var ind = individualsService.getOWLIndividualFor(individualId, ont);
        if (ind == null) {
            throw new NotFoundException("No individual " + individualId);
        }

        var shortForm = kit.getShortFormProvider().getShortForm(ind);

        OWLDataFactory df = ont.getOWLOntologyManager().getOWLDataFactory();

        ProxyBuilder proxyBuilder = new ProxyBuilder(df);

        var top = createGraph(ind, topWithProperties, ont, kit.getFinder(), proxyBuilder);
        var left = createGraph(ind, leftWithProperties, ont, kit.getFinder(), proxyBuilder);
        var right = createGraph(ind, rightWithProperties, ont, kit.getFinder(), proxyBuilder);
        var bottom = bottomWithProperties.isEmpty() // default to all others if not specified
                ? new GraphBuilder(ont, proxyBuilder).withoutProperties(top, left, right).addIndividual(ind)
                : createGraph(ind, bottomWithProperties, ont, kit.getFinder(), proxyBuilder);

        model.addAttribute("title", shortForm);
        model.addAttribute("subject", ind);
        model.addAttribute("proxies", proxyBuilder);
        model.addAttribute("top", top.build());
        model.addAttribute("left", left.build());
        model.addAttribute("right", right.build());
        model.addAttribute("bottom", bottom.build());
        model.addAttribute("mos", rendererFactory.getRenderer(ont).withURLScheme(new GraphURLScheme()));
        model.addAttribute("sfp", kit.getShortFormProvider());

        return new ModelAndView("graph");
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
        var cls = classesService.getOWLClassFor(classId, ont);
        if (cls == null) {
            throw new NotFoundException("No class " + classId);
        }

        var shortForm = kit.getShortFormProvider().getShortForm(cls);

        OWLDataFactory df = ont.getOWLOntologyManager().getOWLDataFactory();

        ProxyBuilder proxyBuilder = new ProxyBuilder(df);

        var top = createGraph(cls, topWithProperties, ont, kit.getFinder(), proxyBuilder);
        var left = createGraph(cls, leftWithProperties, ont, kit.getFinder(), proxyBuilder);
        var bottom = createGraph(cls, bottomWithProperties, ont, kit.getFinder(), proxyBuilder);
        var right = rightWithProperties.isEmpty() // default to all others if not specified
                ? new GraphBuilder(ont, proxyBuilder).withoutProperties(top, left, bottom).addClass(cls)
                : createGraph(cls, rightWithProperties, ont, kit.getFinder(), proxyBuilder);

        model.addAttribute("title", shortForm);
        model.addAttribute("subject", cls);
        model.addAttribute("proxies", proxyBuilder);
        model.addAttribute("top", top.build());
        model.addAttribute("left", left.build());
        model.addAttribute("right", right.build());
        model.addAttribute("bottom", bottom.build());
        model.addAttribute("mos", rendererFactory.getRenderer(ont).withURLScheme(new GraphURLScheme()));
        model.addAttribute("sfp", kit.getShortFormProvider());

        return new ModelAndView("graph");
    }

    private GraphBuilder createGraph(
            OWLNamedIndividual ind,
            List<String> propNames,
            OWLOntology ont,
            OWLEntityFinder finder,
            ProxyBuilder proxyBuilder) {

        Set<OWLProperty> props = propNames.stream()
                .map(name -> getProps(name, finder, ont))
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());

        var builder = new GraphBuilder(ont, proxyBuilder);
        if (!props.isEmpty()) {
            builder.addIndividual(ind).withProperties(props);
        }
        return builder;
    }


    private GraphBuilder createGraph(
            OWLClass cls,
            List<String> propNames,
            OWLOntology ont,
            OWLEntityFinder finder,
            ProxyBuilder proxyBuilder) {

        Set<OWLProperty> props = propNames.stream()
                .map(name -> getProps(name, finder, ont))
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());

        var builder = new GraphBuilder(ont, proxyBuilder);
        if (!props.isEmpty()) {
            builder.addClass(cls).withProperties(props);
        }
        return builder;
    }

    private Set<OWLObjectProperty> getProps(String name, OWLEntityFinder finder, OWLOntology ont) {
        if (name.equals("type")){
            var type = ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE);
            return Set.of(type);
        }
        return finder.getOWLObjectProperties(name);
    }
}
