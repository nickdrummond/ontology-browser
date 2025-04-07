package org.ontbrowser.www.feature.graph;

import com.google.common.collect.Streams;
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
import java.util.stream.Stream;

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

    // TODO
    // property or properties condition (list of properties)
    // subject condition "x type Y"
    // object condition (same)
    @GetMapping(value = "/data", produces = MediaType.APPLICATION_JSON_VALUE)
    public CytoscapeGraph individualJson(
            @RequestParam(required = false, defaultValue = "") final List<String> indivs,
            @RequestParam(required = false) final String query,
            @RequestParam(required = false) final String subtract,
            @RequestParam(required = false, defaultValue = "") final List<String> props,
            @RequestParam(required = false, defaultValue = "") final List<String> without,
            @RequestParam(required = false, defaultValue = "") final List<String> parents,
            @RequestParam(required = false, defaultValue = "") final List<String> follow,
            @RequestParam(required = false, defaultValue = "1") final int depth,
            @ModelAttribute final OWLOntology ont
    ) throws ExecutionException, InterruptedException {
        var finder = kit.getFinder();
        var graphBuilder = new AxiomGraphBuilder();

        if (!props.isEmpty()) {
            var properties = getProps(props, ont, finder);
            // get rid of any properties not in the list
            graphBuilder.addVeto(edge -> !properties.contains(edge.predicate()));
        }

        if (!without.isEmpty()) {
            var withoutProperties = getProps(without, ont, finder);
            // get rid of further properties
            graphBuilder.addVeto(edge -> withoutProperties.contains(edge.predicate()));
        }

        var parentProperties = parents.isEmpty() ? Set.<OWLProperty>of() : getProps(parents, ont, finder);

        var objects = new HashSet<OWLEntity>();
        if (query != null && !query.isBlank()) {
            objects.addAll(getInds(query));
        }
        if (indivs != null && !indivs.isEmpty()) {
            objects.addAll(getInds(indivs, finder));
        }
        if (subtract != null && !subtract.isBlank()) {
            var subtractInds = getInds(subtract);
            objects.removeAll(subtractInds);
            graphBuilder.addVeto(edge -> subtractInds.contains(edge.subject()) || subtractInds.contains(edge.object()));
        }
        if (objects.isEmpty() && !props.isEmpty()) {
            var properties = getProps(props, ont, finder);
            objects.addAll(properties);
        }

        for (var obj : objects) {
            var axioms = ont.getReferencingAxioms(obj, Imports.INCLUDED);
            if (follow != null && !follow.isEmpty() && depth > 1) {
                var followProps = getProps(props, ont, finder);
                if (!followProps.isEmpty()) {
                    // TODO needs to know the algorithm for generating edges to follow !?
//                    addFollow(obj, followProps, depth);
                }
            }
            graphBuilder.addAxioms(axioms);
        }

        var df = ont.getOWLOntologyManager().getOWLDataFactory();
        var renderer = new MOSStringRenderer(kit.getFinder(), ont);
        return new CytoscapeGraph(graphBuilder.build(), df, renderer, parentProperties, objects);
    }

//            // TODO if no query or indivs, get all with property
//    @GetMapping(value = "/data", produces = MediaType.APPLICATION_JSON_VALUE)
//    public CytoscapeGraph individualJson(
//            @RequestParam(required = false, defaultValue = "") final List<String> indivs,
//            @RequestParam(required = false) final String query,
//            @RequestParam(required = false, defaultValue = "") final List<String> props,
//            @RequestParam(required = false, defaultValue = "") final List<String> parents,
//            @RequestParam(required = false, defaultValue = "") final List<String> without,
//            @RequestParam(required = false, defaultValue = "") final List<String> follow,
//            @RequestParam(required = false, defaultValue = "1") final int depth,
//            @ModelAttribute final OWLOntology ont
//    ) throws ExecutionException, InterruptedException {
//        var finder = kit.getFinder();
//
//        var parentProperties = getProps(parents, ont, finder);
//        var followProperties = getProps(follow, ont, finder);
//        var withoutProperties = getProps(without, ont, finder);
//        var properties = props.isEmpty()
//                ? getAllProps(ont)
//                : getProps(props, ont, finder);
//
//        //
//        Set<OWLObject> objects = new HashSet<>();
//        if (query != null && !query.isBlank()) {
//            objects.addAll(getInds(query));
//        }
//        if (indivs != null && !indivs.isEmpty()) {
//            objects.addAll(getInds(indivs, finder));
//        }
//        if (objects.isEmpty() && !props.isEmpty()) {
//            objects.addAll(properties);
//        }
//
//        var descr = new GraphDescriptor(ont)
//                .addObjects(objects)
//                .withProperties(properties)
//                .withProperties(parentProperties)
//                //.withInverseProperties(parentProperties) // adding this "fills" parents with their children
//                .withFollow(followProperties)
//                .withoutProperties(withoutProperties)
//                .withDepth(depth);
//
//        var graph = new GraphBuilder(descr).build();
//
//        var df = ont.getOWLOntologyManager().getOWLDataFactory();
//        var renderer = new MOSStringRenderer(kit.getFinder(), ont);
//        return new CytoscapeGraph(graph, df, renderer, parentProperties, objects);
//    }

    private Set<? extends OWLProperty> getAllProps(OWLOntology ont) {
        return Streams.concat(
                Stream.of(ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE.getIRI())),
                ont.objectPropertiesInSignature(Imports.INCLUDED),
                ont.dataPropertiesInSignature(Imports.INCLUDED)
        ).collect(Collectors.toSet());
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

    private Set<? extends OWLProperty> getProps(String name, OWLEntityFinder finder, OWLOntology ont) {
        if (name.equals("type")) {
            var type = ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE);
            return Set.of(type);
        }
        Set<? extends OWLProperty> props = finder.getOWLObjectProperties(name);
        if (props.isEmpty()) {
            props = finder.getOWLDataProperties(name);
        }
        return props;
    }
}
