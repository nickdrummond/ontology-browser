package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.dlquery.DLQuery;
import org.ontbrowser.www.feature.dlquery.ParserService;
import org.ontbrowser.www.feature.dlquery.QueryType;
import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.http.MediaType;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

@RestController
@Profile("graph")
@RequestMapping(value = "/graph")
public class CytoscapeController extends ApplicationController {

    private final ParserService parserService;
    private final ReasonerService reasonerService;

    public CytoscapeController(@Autowired ParserService parserService,
                               @Autowired ReasonerService reasonerService) {
        this.parserService = parserService;
        this.reasonerService = reasonerService;
    }

    @GetMapping()
    public ModelAndView individual(
            @ModelAttribute final OWLOntology ont,
            Model model
    ) {
        model.addAttribute("title", "Graph");
        return new ModelAndView("cytoscape");
    }

    // TODO if no query or indivs, get all with property
    @GetMapping(value = "/data", produces = MediaType.APPLICATION_JSON_VALUE)
    public CytoscapeGraph individualJson(
            @RequestParam(required = false, defaultValue = "") final List<String> indivs,
            @RequestParam(required = false) final String query,
            @RequestParam(required = false, defaultValue = "") final List<String> props,
            @RequestParam(required = false, defaultValue = "") final List<String> parents,
            @RequestParam(required = false, defaultValue = "") final List<String> follow,
            @RequestParam(required = false, defaultValue = "") final List<String> without,
            @RequestParam(required = false, defaultValue = "1") final int depth,
            @ModelAttribute final OWLOntology ont
    ) throws ExecutionException, InterruptedException {
        var finder = kit.getFinder();

        var parentProperties = getProps(parents, ont, finder);
        var followProperties = getProps(follow, ont, finder);
        var withoutProperties = getProps(without, ont, finder);
        var properties = props.isEmpty()
                ? ont.getObjectPropertiesInSignature(Imports.INCLUDED)
                : getProps(props, ont, finder);

        //
        Set<OWLObject> objects = new HashSet<>();
        if (query != null && !query.isBlank()) {
            objects.addAll(getInds(query));
        }
        if (indivs != null && !indivs.isEmpty()) {
            objects.addAll(getInds(indivs, finder));
        }
        if (objects.isEmpty() && !props.isEmpty()) {
            objects.addAll(properties);
        }

        var descr = new GraphDescriptor(ont)
                .addObjects(objects)
                .withProperties(properties)
                .withProperties(parentProperties)
                .withInverseProperties(parentProperties)
                .withFollow(followProperties)
                .withoutProperties(withoutProperties)
                .withDepth(depth);

        var graph = new GraphBuilder(descr).build();

        var df = ont.getOWLOntologyManager().getOWLDataFactory();
        var renderer = new MOSStringRenderer(kit.getFinder(), ont);
        return new CytoscapeGraph(graph, df, renderer, parentProperties, objects);
    }

    private Set<OWLNamedIndividual> getInds(String query) throws ExecutionException, InterruptedException {
        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var owlEntityChecker = kit.getOWLEntityChecker();
        var clsEpr = parserService.getOWLClassExpression(query, df, owlEntityChecker);
        var dlQuery = new DLQuery(clsEpr, QueryType.instances);
        return reasonerService.query(dlQuery).stream().map(OWLEntity::asOWLNamedIndividual).collect(Collectors.toSet());
    }

    private Set<OWLNamedIndividual> getInds(List<String> names, OWLEntityFinder finder) {
        return names.stream()
                .map(name -> getInds(name, finder))
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());
    }

    private Set<OWLNamedIndividual> getInds(String name, OWLEntityFinder finder) {
        return finder.getOWLIndividuals(name);
    }

    private Set<OWLProperty> getProps(List<String> propNames, OWLOntology ont, OWLEntityFinder finder) {
        return propNames.stream()
                .map(name -> getProps(name, finder, ont))
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());
    }

    private Set<OWLObjectProperty> getProps(String name, OWLEntityFinder finder, OWLOntology ont) {
        if (name.equals("type")){
            var type = ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE);
            return Set.of(type);
        }
        return finder.getOWLObjectProperties(name);
    }
}
