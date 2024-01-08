package org.coode.www.model.timeline;

import com.google.common.collect.Streams;
import org.coode.www.model.Tree;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

import static org.coode.www.model.timeline.EventUtils.*;

public class EventFactory {

    public final Logger logger = LoggerFactory.getLogger(EventFactory.class);

    private final ShortFormProvider sfp;
    private final AbstractRelationsHierarchyService<OWLObjectProperty> duringTree;
    private final AbstractRelationsHierarchyService<OWLObjectProperty> afterTree;

    public EventFactory(
            AbstractRelationsHierarchyService<OWLObjectProperty> duringTree,
            AbstractRelationsHierarchyService<OWLObjectProperty> afterTree,
            ShortFormProvider sfp) {
        this.duringTree = duringTree;
        this.afterTree = afterTree;
        this.sfp = sfp;
    }

    public Timeline buildTimeline(
            OWLNamedIndividual event,
            int depth) {
        return buildTimelineFor(afterTree.getSubtree(event), depth, false, false);
    }

    public Timeline buildTimelineFor(
            Tree<OWLNamedIndividual> eventTree,
            int depth,
            boolean diverge,
            boolean converge) {
        logger.info("Tree {}", eventTree);
        List<TConn> chain = buildChainFrom(new ArrayList<>(), eventTree, depth);
        return new Timeline(SOMETIME_AFTER, chain, diverge, converge);
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

        return new TParent(label, buildTimeline(subevents, depth - 1));
    }

    private Timeline buildTimeline(List<OWLNamedIndividual> events, int depth) {

        Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree =
                events.stream().collect(Collectors.toMap(ev -> ev, afterTree::getChildren));

        event2Tree.forEach((k, v) -> logger.info("TREE {}", v));

        Set<OWLNamedIndividual> roots = getRoots(event2Tree);

        if (roots.isEmpty()) {
            logger.error("No roots found");
        } else if (roots.size() == 1) {
            OWLNamedIndividual root = roots.iterator().next();

            logger.info("Single root found {}", root);

            return buildTimelineFrom(root, event2Tree, depth, false, false);
        }

        logger.info("Multiple roots found {}", roots.size());

        return new Timeline(SOMETIME_AFTER, List.of(new TConn(buildParallel(roots, event2Tree, depth), AFTER)), false, false);
    }

    private Timeline buildTimelineFrom(
            OWLNamedIndividual current,
            Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree,
            int depth,
            boolean diverge,
            boolean converge) {
        List<TConn> chain = buildChainOnlyUsingGivenEventsFrom(new ArrayList<>(), current, event2Tree, depth);
        return new Timeline(diverge ? AFTER : SOMETIME_AFTER, chain, diverge, converge);
    }

    // TODO sometime...
    // TODO detect cycles?
    private List<TConn> buildChainOnlyUsingGivenEventsFrom(
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
            return buildChainOnlyUsingGivenEventsFrom(chain, treeNode.children.get(0).value.get(0), event2Tree, depth);
        }

        logger.info("Divergent path in chain: {}", current);
        return buildParallel(chain, treeNode, event2Tree, depth);
    }

    public List<TConn> buildParallel(List<TConn> chain, Tree<OWLNamedIndividual> treeNode, Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree, int depth) {

        // TODO make chain immutable - return copy
        List<List<TConn>> divergentChains = treeNode.children.stream()
                .map(t -> buildChainOnlyUsingGivenEventsFrom(new ArrayList<>(), t.value.get(0), event2Tree, depth))
                .toList();

            buildConverging(chain, divergentChains);
        return chain;
    }

    private List<TConn> buildChainFrom(List<TConn> chain, Tree<OWLNamedIndividual> treeNode, int depth) {

        OWLNamedIndividual current = treeNode.value.get(0);

        chain.add(new TConn(buildNode(current, depth), AFTER));

        if (treeNode.children.isEmpty()) {
            logger.info("Last node in chain: {}", current);
            return chain;
        }

        if (treeNode.children.size() == 1) {
            logger.info("Single path in chain: {}", current);
            return buildChainFrom(chain, treeNode.children.get(0), depth);
        }

        // TODO any convergence has to be detected on the timelines
        // see Battle_of_Yavin
        logger.info("Divergent path in chain: {}", current);
        List<Timeline> divergentTimelines = treeNode.children.stream()
                .map(t -> buildTimelineFor(t, depth, true, false))
                .toList();

        chain.add(new TConn(divergentTimelines, AFTER));

        return chain;
    }

    private List<Timeline> buildParallel(
            Set<OWLNamedIndividual> roots,
            Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree,
            int depth) {

        // TODO sort by longest first

        return roots.stream()
                .map(root -> buildTimelineFrom(root, event2Tree, depth, false, false))
                .sorted(Comparator.comparing(timeline -> -timeline.events().size()))
                .toList();
    }

    private Set<OWLNamedIndividual> getRoots(Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree) {
        // TODO also check for any which have roots NOT included in the parent
        return event2Tree.keySet().stream()
                .filter(e -> !isChildIn(e, event2Tree))
                .collect(Collectors.toSet());
    }

    private boolean isChildIn(OWLNamedIndividual event, Map<OWLNamedIndividual, Tree<OWLNamedIndividual>> event2Tree) {
        for (Tree<OWLNamedIndividual> parent : event2Tree.values()) {
            for (Tree<OWLNamedIndividual> child : parent.children) {
                if (child.value.contains(event)) {
                    return true;
                }
            }
        }
        return false;
    }

    private String getLabel(OWLNamedIndividual event) {
        return sfp.getShortForm(event);
    }
}