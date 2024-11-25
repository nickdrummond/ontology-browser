package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.entities.OWLIndividualsService;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
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
import java.util.stream.Collectors;

@RestController
@Profile("graph")
@RequestMapping(value = "/graph")
public class CytoscapeController extends ApplicationController {

    @GetMapping()
    public ModelAndView individual(
            @ModelAttribute final OWLOntology ont,
            Model model
    ) {
        model.addAttribute("title", "Graph");
        return new ModelAndView("cytoscape");
    }

    @GetMapping(value = "/data", produces = MediaType.APPLICATION_JSON_VALUE)
    public CytoscapeGraph individualJson(
            @RequestParam(required = true) final List<String> indivs,
            @RequestParam(required = false, defaultValue = "") final List<String> props,
            @RequestParam(required = false, defaultValue = "") final List<String> compound,
            @RequestParam(required = false, defaultValue = "") final List<String> follow,
            @RequestParam(required = false, defaultValue = "1") final int depth,
            @ModelAttribute final OWLOntology ont
    ) {
        var proxyBuilder = new ProxyBuilder(ont.getOWLOntologyManager().getOWLDataFactory());
        var finder = kit.getFinder();

        var compoundProperties = getProps(compound, ont, finder);
        var followProperties = getProps(follow, ont, finder);
        var properties = props.isEmpty()
                ? ont.getObjectPropertiesInSignature(Imports.INCLUDED)
                : getProps(props, ont, finder);
        var individuals = getInds(indivs, finder);
        GraphDescriptor descr = new GraphDescriptor(ont)
                .addEntities(individuals)
                .withProperties(properties)
                .withProperties(compoundProperties)
                .withInverseProperties(compoundProperties)
                .withFollow(followProperties)
                .withDepth(depth);
        var graph = new GraphBuilder(proxyBuilder, descr).build();

        return new CytoscapeGraph(graph, kit.getShortFormProvider(), compoundProperties, individuals);
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
