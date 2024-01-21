package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.timeline.*;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.OWLOntologiesService;
import org.coode.www.service.ReasonerService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.RelationsHierarchyService;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;
import java.util.Set;
import java.util.function.Function;


@Controller
@RequestMapping(value = TimelineController.PATH)
public class TimelineController extends ApplicationController {
    static final String PATH = "/timeline";

    private final OWLObjectPropertiesService propertiesService;
    private final OWLOntologiesService ontologiesService;

    @Autowired
    private ReasonerService reasonerService;

    public TimelineController(
            @Autowired OWLObjectPropertiesService propertiesService,
            @Autowired OWLOntologiesService ontologiesService
            ) {
        this.propertiesService = propertiesService;
        this.ontologiesService = ontologiesService;
    }


    // TODO highlight events with a given participant
    // TODO YEAR!
    @GetMapping(path = "/starwars")
    public String starwarsTimeline(
            final @RequestParam(defaultValue = "A_long_time_ago") String event,
            final @RequestParam(required = false) String ontId,
            final @RequestParam(required = false) String filterProp,
            final @RequestParam(required = false) String filterObject,
            final @RequestParam(defaultValue = "" + Integer.MAX_VALUE) int depth,
            final Model model) throws NotFoundException {

        OWLEntityChecker checker = kit.getOWLEntityChecker();

        OWLNamedIndividual target = checker.getOWLIndividual(event);

        if (target == null) {
            throw new NotFoundException(event + " event is unknown");
        }

        OWLOntology ont = (ontId != null) ? ontologiesService.getOntologyFor(ontId, kit) : kit.getActiveOntology();

        OWLObjectProperty duringProp = checker.getOWLObjectProperty("during");
        OWLObjectProperty afterProp = checker.getOWLObjectProperty("after");
        OWLObjectProperty sometimeAfterProp = checker.getOWLObjectProperty("sometimeAfter");

        AbstractRelationsHierarchyService<OWLObjectProperty> duringTree = propertiesService
                .getRelationsHierarchy(null)
                .withProperties(duringProp, ont, true);

        AbstractRelationsHierarchyService<OWLObjectProperty> afterTree = propertiesService
                .getRelationsHierarchy(null)
                .withProperties(afterProp, ont, true)
                .withMoreProperties(sometimeAfterProp);

        OWLDataProperty year = checker.getOWLDataProperty("year");

        Function<OWLNamedIndividual, String> yearProvider = (OWLNamedIndividual ind) ->
                EventUtils.getInteger(ind, year, ont).map(Object::toString).orElse("");

        if (filterProp != null) {
            OWLObjectProperty fProp = checker.getOWLObjectProperty(filterProp);
            if (fProp == null) {
                throw new NotFoundException(fProp + " filter property is unknown");
            }
            Set<OWLObjectPropertyExpression> fProps = reasonerService.getReasoner().getSubObjectProperties(fProp, false).getFlattened();
            fProps.add(fProp);
            OWLNamedIndividual fInd = checker.getOWLIndividual(filterObject);
            if (fInd == null) {
                throw new NotFoundException(fInd + " filter object is unknown");
            }

            // Return additional classnames for ind
            Function<OWLNamedIndividual, String> filter = (OWLNamedIndividual ind) ->
                    EventUtils.filterByRelations(ind, fProps, fInd, ont) ? "filter1" : "";

            model.addAttribute("filter", filter);
        }
        else {
            model.addAttribute("filter", null);
        }

        EventFactory fac = new EventFactory(duringTree, afterTree);

        model.addAttribute("title", "Timeline");
        model.addAttribute("root", fac.buildTimeline(target, depth));

        // TODO link to the filtered timeline page?
        OWLHTMLRenderer ren = new OWLHTMLRenderer(kit).withBreakOnUnderscore(false);

        model.addAttribute("ren", ren);
        model.addAttribute("getYear", yearProvider);

        return "timeline";
    }

    @GetMapping(path = "/tests")
    public String testTimelines(final Model model) throws OWLOntologyCreationException {

        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
        OWLDataFactory df = mngr.getOWLDataFactory();

        OWLOntology ont = mngr.createOntology();

        OWLObjectProperty during = df.getOWLObjectProperty("during");
        OWLObjectProperty after = df.getOWLObjectProperty("after");

        AbstractRelationsHierarchyService<OWLObjectProperty> duringTree = new RelationsHierarchyService()
                .withProperties(during, ont, true);

        AbstractRelationsHierarchyService<OWLObjectProperty> afterTree = new RelationsHierarchyService()
                .withProperties(after, ont, true);

        EventFactory eventFactory = new EventFactory(duringTree, afterTree);

        OWLNamedIndividual parent = df.getOWLNamedIndividual("Parent");
        OWLNamedIndividual child1 = df.getOWLNamedIndividual("Child1");
        OWLNamedIndividual child2 = df.getOWLNamedIndividual("Child2");
        OWLNamedIndividual child3 = df.getOWLNamedIndividual("Child3");
        OWLNamedIndividual p2_1 = df.getOWLNamedIndividual("p2_1");
        OWLNamedIndividual p2_2 = df.getOWLNamedIndividual("p2_2");
        OWLNamedIndividual p2_3 = df.getOWLNamedIndividual("p2_3");
        OWLNamedIndividual p3_1 = df.getOWLNamedIndividual("p3_1");
        OWLNamedIndividual p3_2 = df.getOWLNamedIndividual("p3_2");

        // During
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, child1, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, child2, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, child3, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p2_1, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p2_2, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p2_3, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p3_1, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p3_2, parent));

        // After
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, child2, child1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, child3, child2));

        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p2_1, child1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p2_2, p2_1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p2_3, p2_2));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, child3, p2_3));

        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p3_1, child1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p3_2, p3_1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p2_3, p3_2));

        Timeline<OWLNamedIndividual, OWLObjectProperty> timeline = eventFactory.buildTimeline(parent, Integer.MAX_VALUE);

        model.addAttribute("title", "Timeline");
        model.addAttribute("root", timeline);

        return "timeline";
    }

    @GetMapping
    public String timeline(final Model model) {

        model.addAttribute("title", "Timeline");

        TProp after = new TProp("after");
        TProp sometimeAfter = new TProp("sometimeAfter");

        model.addAttribute("root", new Timeline(
                List.of(
                        new TConn(after, new TParent("Parent", new Timeline(
                                List.of(
                                        new TConn(after, "Child A"),
                                        new TConn(after, List.of(
                                                new Timeline(List.of(
                                                        new TConn(after, "Child B")
                                                ), after, false, false),
                                                new Timeline(
                                                        List.of(
                                                                new TConn(after, "P A"),
                                                                new TConn(sometimeAfter, "P B")
                                                        ), after,true, false),
                                                new Timeline(
                                                        List.of(
                                                                new TConn(after, "P2 A"),
                                                                new TConn(sometimeAfter, new TParent("P2 B",
                                                                        new Timeline(List.of(
                                                                                new TConn(after, "P2 B1"),
                                                                                new TConn(after, "P2 B2")
                                                                        ), after, false, false)))
                                                        ), after,true, false)
                                        )),
                                        new TConn(after, "Child C")
                                ), after,false, false)
                        ))
                ), after,
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
}
