package org.coode.www.controller;

import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.timeline.*;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.OWLOntologiesService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.PropComparator;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.*;


@Controller
@RequestMapping(value = TimelineController.PATH)
public class TimelineController extends ApplicationController {
    static final String PATH = "/timeline";

    private final OWLObjectPropertiesService propertiesService;
    private final OWLOntologiesService ontologiesService;

    public TimelineController(
            @Autowired OWLObjectPropertiesService propertiesService,
            @Autowired OWLOntologiesService ontologiesService
            ) {
        this.propertiesService = propertiesService;
        this.ontologiesService = ontologiesService;
    }

    @GetMapping
    public String timeline(final Model model) {

        model.addAttribute("title", "Timeline");

        TProp after = new TProp("after");
        TProp sometimeAfter = new TProp("sometimeAfter");

        model.addAttribute("root", new Timeline(
                after,
                List.of(
                        new TConn(new TParent("Parent", new Timeline(after,
                                        List.of(
                                                new TConn("Child A", after),
                                                new TConn(List.of(
                                                        new Timeline(after, List.of(
                                                                new TConn("Child B", after)
                                                                ), false, false),
                                                        new Timeline(after,
                                                                List.of(
                                                                        new TConn("P A", after),
                                                                        new TConn("P B", sometimeAfter)
                                                                ), true, false),
                                                        new Timeline(after,
                                                                List.of(
                                                                        new TConn("P2 A", after),
                                                                        new TConn(new TParent("P2 B",
                                                                                new Timeline(after, List.of(
                                                                                        new TConn("P2 B1", after),
                                                                                        new TConn("P2 B2", after)
                                                                                ), false, false)), sometimeAfter)
                                                                ), true, false)
                                                ), after),
                                                new TConn("Child C", after)
                                        ), false, false)
                        ), after)
                ),
                false, false));

//        model.addAttribute("root", new Timeline(
//                after,
//                List.of(
//                        new TConn("Event before", after),
//                        new TConn(new TParent("Parent", new Timeline(after,
//                                List.of(
//                                        new TConn("Child A", after),
//                                        new TConn("Child B", after),
//                                        new TConn("Child C", after)
//                                ), false, false)
//                        ), after),
//                        new TConn("Event after", after)
//                ),
//                false, false));

        return "timeline";
    }

    @GetMapping(path = "/starwars")
    public String starwarsTimeline(
            final @RequestParam(defaultValue = "A_long_time_ago") String event,
            final @RequestParam() String ontId,
            final @RequestParam(defaultValue = "" + Integer.MAX_VALUE) int depth,
            final Model model) throws NotFoundException {

        OWLEntityChecker checker = kit.getOWLEntityChecker();

        OWLNamedIndividual target = checker.getOWLIndividual(event);

        OWLOntology ont = (ontId != null) ? ontologiesService.getOntologyFor(ontId, kit) : kit.getActiveOntology();

        OWLObjectProperty duringProp = checker.getOWLObjectProperty("during");
        OWLObjectProperty afterProp = checker.getOWLObjectProperty("after");
//        OWLObjectProperty sometimeAfterProp = checker.getOWLObjectProperty("sometimeAfter");

        // TODO revisit if this comparator useful??
        Comparator<Tree<OWLNamedIndividual>> comparator = new PropComparator(afterProp, ont);

        AbstractRelationsHierarchyService<OWLObjectProperty> duringTree = propertiesService
                .getRelationsHierarchy(comparator)
                .withProperties(duringProp, ont, true);

        AbstractRelationsHierarchyService<OWLObjectProperty> afterTree = propertiesService
                .getRelationsHierarchy(comparator)
                .withProperties(afterProp, ont, true);

        EventFactory fac = new EventFactory(duringTree, afterTree, kit.getShortFormProvider());
//
//        if (filter != null) {
//            OWLNamedIndividual ind = kit.getOWLEntityChecker().getOWLIndividual(filter);
//            if (ind != null) {
//                fac.withParti
//            }
//        }

        model.addAttribute("title", "Timeline");
        model.addAttribute("root", fac.buildTimeline(target, depth));

        return "timeline";
    }
}
