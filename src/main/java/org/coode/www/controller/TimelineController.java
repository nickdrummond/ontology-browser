package org.coode.www.controller;

import org.coode.www.model.timeline.*;
import org.coode.www.service.OWLObjectPropertiesService;
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

    public TimelineController(
            @Autowired OWLObjectPropertiesService propertiesService
    ) {
        this.propertiesService = propertiesService;
    }

    @GetMapping
    public String timeline(final Model model) {

        model.addAttribute("title", "Timeline");

        TProp before = new TProp("before");
        TProp dashed = new TProp("dashed");

        model.addAttribute("root", new Timeline(
                before,
                List.of(
                        new TConn("Event before", before),
                        new TConn(new TParent("Parent", new Timeline(before,
                                        List.of(
                                                new TConn("Child A", before),
                                                new TConn("Child B", before),
                                                new TConn("Child C", before)
                                        ), false, false)
                        ), before),
                        new TConn("Event after", before)
                ),
                false, false));


        model.addAttribute("root2", new Timeline(
                before,
                List.of(
                        new TConn(new TParent("Parent", new Timeline(before,
                                        List.of(
                                                new TConn("Child A", before),
                                                new TConn(List.of(
                                                        new Timeline(before, List.of(
                                                                new TConn("Child B", before)
                                                                ), false, false),
                                                        new Timeline(before,
                                                                List.of(
                                                                        new TConn("P A", before),
                                                                        new TConn("P B", dashed)
                                                                ), true, false),
                                                        new Timeline(before,
                                                                List.of(
                                                                        new TConn("P2 A", before),
                                                                        new TConn(new TParent("P2 B",
                                                                                new Timeline(before, List.of(
                                                                                        new TConn("P2 B1", before),
                                                                                        new TConn("P2 B2", before)
                                                                                ), false, false)), dashed)
                                                                ), true, false)
                                                ), before),
                                                new TConn("Child C", before)
                                        ), false, false)
                        ), before)
                ),
                false, false));

        return "timeline";
    }

    @GetMapping(path = "/starwars")
    public String starwarsTimeline(
            final @RequestParam(defaultValue = "A_long_time_ago") String event,
            final @RequestParam(defaultValue = "" + Integer.MAX_VALUE) int depth,
            final Model model) {

        EventFactory fac = new EventFactory(kit, propertiesService);

        OWLNamedIndividual target = kit.getFinder().getOWLIndividuals(event).iterator().next();

        final TParent root = fac.buildEvent(target, depth);

        model.addAttribute("title", "Timeline");
        model.addAttribute("root", root);

        return "timeline";
    }
}
