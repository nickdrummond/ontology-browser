package org.coode.www.model.timeline;

import com.google.common.collect.Streams;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.PropComparator;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class EventFactory {

    public final Logger logger = LoggerFactory.getLogger(EventFactory.class);

    public static final TProp AFTER = new TProp("after");
    public static final TProp SOMETIME_AFTER = new TProp("sometimeAfter");
    private final OWLHTMLKit kit;

    private final AbstractRelationsHierarchyService<OWLObjectProperty> duringTree;
    private final AbstractRelationsHierarchyService<OWLObjectProperty> afterTree;

    // TODO
//    private final AbstractRelationsHierarchyService<OWLObjectProperty> sometimeAfterTree;

    private final OWLDataProperty startProp;
    private final OWLDataProperty yearProp;
    private final OWLObjectProperty duringProp;
    private final OWLObjectProperty afterProp;
    private final OWLObjectProperty sometimeAfterProp;
    private final OWLObjectProperty participantProp;

//    private final Map<OWLNamedIndividual, TNode> cache = new HashMap<>();
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

        // TODO revisit if this comparator useful??
        Comparator<Tree<OWLNamedIndividual>> comparator = new PropComparator(afterProp, kit.getActiveOntology());

        this.duringTree = propertiesService
                .getRelationsHierarchy(comparator)
                .withProperties(duringProp, kit.getActiveOntology(), true);

        this.afterTree = propertiesService
                .getRelationsHierarchy(comparator)
                .withProperties(afterProp, kit.getActiveOntology(), true);
    }

//
//    public Timeline buildTimeline(
//            OWLNamedIndividual event,
//            int depth) {
//
//        Tree<OWLNamedIndividual> followingEvents = afterTree.getSubtree(event);
//        afterTree.
//
//        return getTimelineFrom(event, followingEvents, depth, false, false);
//    }

    public Timeline buildTimeline(
            OWLNamedIndividual event,
            int depth) {

        TNode node = buildNode(event, depth);
        return new Timeline(AFTER, List.of(new TConn(node, AFTER)), false, false);
    }

    private TNode buildNode(OWLNamedIndividual event, int depth) {
        String label = getLabel(event);
        if (depth <= 1) { // don't go any further
            return new TEvent(label);
        }

        Tree<OWLNamedIndividual> children = duringTree.getChildren(event);

        if (children.childCount == 0) { // simple
            return new TEvent(label);
        }

        List<OWLNamedIndividual> subevents = children.children.stream()
                .map(t -> t.value)
                .flatMap(Streams::stream).toList();

        return new TParent(label, buildTimeline(subevents, depth-1));
    }

    private Timeline buildTimeline(List<OWLNamedIndividual> events, int depth) {

        Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree =
                events.stream().collect(Collectors.toMap(ev -> ev, afterTree::getChildren));

        event2Tree.forEach((k, v) -> logger.info("TREE {}", v));

        Set<OWLNamedIndividual> roots = getRoots(event2Tree);

        if (roots.isEmpty()) {
            logger.error("No roots found");
        }
        else if (roots.size() == 1) {
            OWLNamedIndividual root = roots.iterator().next();

            logger.info("Single root found {}", root);

            return getTimelineFrom(root, event2Tree, depth, false, false);
        }

        logger.info("Multiple roots found {}", roots.size());

        return new Timeline(SOMETIME_AFTER, List.of(new TConn(buildParallel(roots, event2Tree, depth), AFTER)), false, false);
    }

    private Timeline getTimelineFrom(
            OWLNamedIndividual current,
            Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree,
            int depth,
            boolean diverge,
            boolean converge) {
        List<TConn> chain = getChain(new ArrayList<>(), current, event2Tree, depth);
        return new Timeline(diverge ? AFTER : SOMETIME_AFTER, chain, diverge, converge);
    }

    // TODO sometime...
    // TODO detect cycles?
    private List<TConn> getChain(
            List<TConn> chain,
            OWLNamedIndividual current,
            Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree,
            int depth) {

        chain.add(new TConn(buildNode(current, depth), AFTER));

        Tree<OWLNamedIndividual> treeNode = event2Tree.get(current);

        if (treeNode == null) {
            logger.error("Not found in parent: {}. Render as faint", current);
            return chain;
        }

        if (treeNode.childCount == 0) {
            logger.info("Last node in chain: {}", current);
            return chain;
        }

        if (treeNode.childCount == 1) {
            logger.info("Single path in chain: {}", current);
            return getChain(chain, treeNode.children.get(0).value.get(0), event2Tree, depth);
        }

        logger.info("Divergent path in chain: {}", current);
        List<Timeline> divergentTimelines  = treeNode.children.stream()
                .map(t -> getTimelineFrom(t.value.get(0), event2Tree, depth, true, false))
                .toList();

        chain.add(new TConn(divergentTimelines, AFTER));

        // TODO any convergence has to be detected on the timelines
        // see Battle_of_Yavin

        return chain;
    }


    private List<Timeline> buildParallel(
            Set<OWLNamedIndividual> roots,
            Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree,
            int depth) {
        // TODO convergence detection
        return roots.stream().map(root -> getTimelineFrom(root, event2Tree, depth, false, false)).toList();
    }

    private Set<OWLNamedIndividual> getRoots(Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree) {
        // TODO also check for any which have roots NOT included in the parent
        return event2Tree.keySet().stream()
                .filter(e -> !isChildIn(e, event2Tree))
                .collect(Collectors.toSet());
    }

    private boolean isChildIn(OWLNamedIndividual event, Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree) {
        for (Tree<OWLNamedIndividual> parent: event2Tree.values()) {
            for (Tree<OWLNamedIndividual> child: parent.children) {
                if (child.value.contains(event)) {
                    return true;
                }
            }
        }
        return false;
    }

//        Optional<Integer> year = getInteger(event, yearProp);
//        Optional<Integer> start = getInteger(event, startProp);
//        List<TParent> after = getRelationship(event, afterProp, depth);
//        List<String> participants = getParticipants(event, participantProp);

////        List<TParent> children = depth > 0 ? getChildren(event, depth - 1) : Collections.emptyList();
//        TParent e = null;//new TParent(label, new Timeline(null, children);//, start, year, after, participants, children);
//        cache.put(event, e);
//    }

    private String getLabel(OWLNamedIndividual event) {
        return kit.getShortFormProvider().getShortForm(event);
    }
//
//    private List<TParent> getRelationship(
//            OWLNamedIndividual event,
//            OWLObjectProperty prop,
//            int depth) {
//        return getRelationships(event, prop)
//                .map(obj -> buildEvent(obj, depth))
//                .toList();
//    }
//
//    private List<String> getParticipants(
//            OWLNamedIndividual event,
//            OWLObjectProperty prop) {
//        return getRelationships(event, prop)
//                .map(this::getLabel)
//                .toList();
//    }
//
//    private Stream<OWLNamedIndividual> getRelationships(
//            OWLNamedIndividual event,
//            OWLObjectProperty prop) {
//        return kit.getActiveOntology().axioms(event, Imports.INCLUDED)
//                .filter(OWLObjectPropertyAssertionAxiom.class::isInstance)
//                .map(OWLObjectPropertyAssertionAxiom.class::cast)
//                .filter(ax -> propertiesService.isEquivalentOrSubproperty(ax.getProperty(), prop, kit.getActiveOntology()))
//                .filter(ax -> ax.getSubject().equals(event))
//                .map(HasObject::getObject)
//                .filter(OWLNamedIndividual.class::isInstance)
//                .map(OWLNamedIndividual.class::cast);
//    }
//
//    private List<TParent> getChildren(
//            OWLNamedIndividual event,
//            int depth) {
//        // TODO group by chains of "after"
//        return duringTree.getChildren(event).children.stream()
//                .map(child -> buildEvent(child.value.iterator().next(), depth))
////                .sorted(Comparator.comparing(e -> e.year().orElse(e.start().orElse(Integer.MAX_VALUE))))
//                .toList();
//    }
//
//    private Optional<Integer> getInteger(
//            OWLNamedIndividual event,
//            OWLDataProperty prop) {
//        return kit.getActiveOntology().importsClosure()
//                .flatMap(ont -> ont.dataPropertyAssertionAxioms(event))
//                .filter(ax -> ax.getProperty().equals(prop))
//                .map(OWLPropertyAssertionAxiom::getObject)
//                .filter(OWLLiteral::isInteger)
//                .map(lit -> Integer.parseInt(lit.getLiteral()))
//                .findFirst();
//    }
}