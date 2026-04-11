package org.ontbrowser.www.feature.graph;

import com.google.common.collect.Streams;
import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.feature.dlquery.DLQuery;
import org.ontbrowser.www.feature.dlquery.QueryType;
import org.ontbrowser.www.feature.parser.ParserService;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.springframework.context.annotation.Profile;
import org.springframework.http.MediaType;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@RestController
@Profile("graph")
@RequestMapping(value = "/graph")
public class GraphController {

    private final BackendContext backend;
    private final ParserService parserService;
//    private final ReasonerService reasonerService;

    public GraphController(
            BackendContext backend,
            ParserService parserService
//            ReasonerService reasonerService
    ) {
        this.backend = backend;
        this.parserService = parserService;
//        this.reasonerService = reasonerService;
    }

    @GetMapping()
    public ModelAndView individual(
            @ModelAttribute final OWLOntology ont,
            Model model
    ) {
        model.addAttribute("title", "Graph");
        return new ModelAndView("graph");
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
            @RequestParam(required = false, defaultValue = "") final List<String> incoming,
            @RequestParam(required = false, defaultValue = "1") final int depth,
            @RequestParam(required = false, defaultValue = "200") final int maxEdges,
            @ModelAttribute final OWLOntology ont
    ) throws ExecutionException, InterruptedException {
        var checker = backend.getOWLEntityChecker();

        var parentProperties = getProps(parents, ont, checker);
        var followProperties = getProps(follow, ont, checker);
        var withoutProperties = getProps(without, ont, checker);
        var incomingProperties = getProps(incoming, ont, checker);
        var properties = props.isEmpty()
                ? getAllProps(ont)
                : getProps(props, ont, checker);

        //
        Set<OWLObject> objects = new HashSet<>();
        if (query != null && !query.isBlank()) {
            objects.addAll(getInds(query));
        }
        if (indivs != null && !indivs.isEmpty()) {
            objects.addAll(getInds(indivs, checker));
        }
        if (objects.isEmpty() && !props.isEmpty()) {
            objects.addAll(properties);
        }

        var descr = new GraphDescriptor(ont)
                .addObjects(objects)
                .withProperties(properties)
                .withProperties(parentProperties)
                .withIncomingProperties(incomingProperties)
                .withFollow(followProperties)
                .withoutProperties(withoutProperties)
                .withMaxEdges(maxEdges)
                .withDepth(depth);

        var graph = new GraphBuilderImpl(descr).build();

        var df = ont.getOWLOntologyManager().getOWLDataFactory();
        var renderer = new MOSStringRenderer(backend.getFinder(), ont);
        return new CytoscapeGraph(graph, df, renderer, parentProperties, objects);
    }

    private Set<? extends OWLProperty> getAllProps(OWLOntology ont) {
        return Streams.concat(
                Stream.of(ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE.getIRI())),
                ont.objectPropertiesInSignature(Imports.INCLUDED),
                ont.dataPropertiesInSignature(Imports.INCLUDED)
        ).collect(Collectors.toSet());
    }

    private Set<OWLNamedIndividual> getInds(String query) throws ExecutionException, InterruptedException {
        var df = backend.getOWLDataFactory();
        var owlEntityChecker = backend.getOWLEntityChecker();
        var clsEpr = parserService.getOWLClassExpression(query, df, owlEntityChecker);
        var dlQuery = new DLQuery(clsEpr, QueryType.instances);
        // TODO in DB mode we have no reasoners
        return Set.of();
//        return reasonerService.query(dlQuery).stream().map(OWLEntity::asOWLNamedIndividual).collect(Collectors.toSet());
    }

    private Set<OWLNamedIndividual> getInds(List<String> names, OWLEntityChecker owlEntityChecker) {
        return names.stream()
                .map(owlEntityChecker::getOWLIndividual)
                .collect(Collectors.toSet());
    }

    private Set<OWLProperty> getProps(List<String> propNames, OWLOntology ont, OWLEntityChecker finder) {
        return propNames.stream()
                .map(name -> getProp(name, finder, ont))
                .collect(Collectors.toSet());
    }

    private OWLProperty getProp(String name, OWLEntityChecker finder, OWLOntology ont) {
        if (name.equals("type")) {
            return ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE);
        }
        OWLProperty prop = finder.getOWLObjectProperty(name);
        if (prop == null) {
            prop = finder.getOWLDataProperty(name);
        }
        return prop;
    }
}
