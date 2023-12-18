package org.coode.www.controller;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.PropComparator;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.*;
import java.util.stream.Stream;

record Event(
        String label,
        Optional<Integer> start,
        Optional<Integer> year,
        List<Event> after,
        List<String> participants,
        List<Event> children
) {
}

class EventFactory {
    private final OWLHTMLKit kit;

    private final AbstractRelationsHierarchyService<OWLObjectProperty> tree;

    private final OWLDataProperty startProp;
    private final OWLDataProperty yearProp;
    private final OWLObjectProperty duringProp;
    private final OWLObjectProperty afterProp;
    private final OWLObjectProperty sometimeAfterProp;
    private final OWLObjectProperty participantProp;

    private final Map<OWLNamedIndividual, Event> cache = new HashMap<>();
    private final OWLObjectPropertiesService propertiesService;

    public EventFactory(final OWLHTMLKit kit, OWLObjectPropertiesService propertiesService) {
        this.kit = kit;
        this.propertiesService = propertiesService;
        this.duringProp = kit.getFinder().getOWLObjectProperties("during").iterator().next();
        this.afterProp = kit.getFinder().getOWLObjectProperties("after").iterator().next();
        this.startProp = kit.getFinder().getOWLDataProperties("starting").iterator().next();
        this.yearProp = kit.getFinder().getOWLDataProperties("year").iterator().next();
        this.sometimeAfterProp = kit.getFinder().getOWLObjectProperties("sometimeAfter").iterator().next();
        this.participantProp = kit.getFinder().getOWLObjectProperties("participant").iterator().next();

        Comparator<Tree<OWLNamedIndividual>> comparator = new PropComparator(afterProp, kit.getActiveOntology());

        this.tree = propertiesService
                .getRelationsHierarchy(comparator)
                .withProperties(duringProp, kit.getActiveOntology(), true);
    }

    public Event buildEvent(
            OWLNamedIndividual event,
            int depth) {

        if (cache.containsKey(event)) {
            return cache.get(event);
        }

        String label = getLabel(event);
        Optional<Integer> year = getInteger(event, yearProp);
        Optional<Integer> start = getInteger(event, startProp);
        List<Event> after = getRelationship(event, afterProp, depth);
        List<String> participants = getParticipants(event, participantProp);

        List<Event> children = depth > 0 ? getChildren(event, depth - 1) : Collections.emptyList();
        Event e = new Event(label, start, year, after, participants, children);
        cache.put(event, e);
        return e;
    }

    private String getLabel(OWLNamedIndividual event) {
        return kit.getShortFormProvider().getShortForm(event);
    }

    private List<Event> getRelationship(
            OWLNamedIndividual event,
            OWLObjectProperty prop,
            int depth) {
        return getRelationships(event, prop)
                .map(obj -> buildEvent(obj, depth))
                .toList();
    }

    private List<String> getParticipants(
            OWLNamedIndividual event,
            OWLObjectProperty prop) {
        return getRelationships(event, prop)
                .map(this::getLabel)
                .toList();
    }

    private Stream<OWLNamedIndividual> getRelationships(
            OWLNamedIndividual event,
            OWLObjectProperty prop) {
        return kit.getActiveOntology().axioms(event, Imports.INCLUDED)
                .filter(OWLObjectPropertyAssertionAxiom.class::isInstance)
                .map(OWLObjectPropertyAssertionAxiom.class::cast)
                .filter(ax -> propertiesService.isEquivalentOrSubproperty(ax.getProperty(), prop, kit.getActiveOntology()))
                .filter(ax -> ax.getSubject().equals(event))
                .map(HasObject::getObject)
                .filter(OWLNamedIndividual.class::isInstance)
                .map(OWLNamedIndividual.class::cast);
    }

    private List<Event> getChildren(
            OWLNamedIndividual event,
            int depth) {
        // TODO group by chains of "after"
        return tree.getChildren(event).children.stream()
                .map(child -> buildEvent(child.value.iterator().next(), depth))
//                .sorted(Comparator.comparing(e -> e.year().orElse(e.start().orElse(Integer.MAX_VALUE))))
                .toList();
    }

    private Optional<Integer> getInteger(
            OWLNamedIndividual event,
            OWLDataProperty prop) {
        return kit.getActiveOntology().importsClosure()
                .flatMap(ont -> ont.dataPropertyAssertionAxioms(event))
                .filter(ax -> ax.getProperty().equals(prop))
                .map(OWLPropertyAssertionAxiom::getObject)
                .filter(OWLLiteral::isInteger)
                .map(lit -> Integer.parseInt(lit.getLiteral()))
                .findFirst();
    }
}

@Controller
@RequestMapping(value = TimelineController.PATH)
public class TimelineController extends ApplicationController {
    static final String PATH = "/timeline";

    private final OWLObjectPropertiesService propertiesService;

    public TimelineController(
            @Autowired OWLObjectPropertiesService propertiesService
    ) {
        this.propertiesService = propertiesService;
    }

    @GetMapping
    public String timeline(
            final @RequestParam(defaultValue = "A_long_time_ago") String event,
            final @RequestParam(defaultValue = "" + Integer.MAX_VALUE) int depth,
            final Model model) {

        EventFactory fac = new EventFactory(kit, propertiesService);

        OWLNamedIndividual target = kit.getFinder().getOWLIndividuals(event).iterator().next();

        final Event root = fac.buildEvent(target, depth);

        model.addAttribute("title", "Timeline");
        model.addAttribute("root", root);

        return "timeline";
    }

}
