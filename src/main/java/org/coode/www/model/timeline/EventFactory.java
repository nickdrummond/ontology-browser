package org.coode.www.model.timeline;

import com.google.common.collect.Streams;
import org.coode.www.model.Tree;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.Relation;
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
    private final HashMap<OWLObjectProperty, TProp> props;

    public EventFactory(
            AbstractRelationsHierarchyService<OWLObjectProperty> duringTree,
            AbstractRelationsHierarchyService<OWLObjectProperty> afterTree,
            ShortFormProvider sfp) {
        this.duringTree = duringTree;
        this.afterTree = afterTree;
        this.props = new HashMap<>();
        afterTree.getProperties().forEach(p -> this.props.put(p, new TProp(sfp.getShortForm(p))));
        duringTree.getProperties().forEach(p -> this.props.put(p, new TProp(sfp.getShortForm(p))));
        this.sfp = sfp;
    }

    public Timeline buildTimeline(
            OWLNamedIndividual event,
            int depth) {
        // TODO Should be able to ask for the subtree without the property
        return buildTimelineFor(afterTree.getSubtree(new Relation<>(afterTree.getProperty(), event)), depth, false, false);
    }

    public Timeline buildTimelineFor(
            Tree<Relation<OWLObjectProperty>> eventTree,
            int depth,
            boolean diverge,
            boolean converge) {
        logger.info("buildTimelineFor: {}", eventTree);
        List<TConn> chain = buildChainFrom(new ArrayList<>(), eventTree, depth);
        return new Timeline(SOMETIME_AFTER_REMOVE, chain, diverge, converge);
    }

    private TNode buildNode(Relation<OWLObjectProperty> event, int depth) {
        String label = getLabel(event.individual());

        if (depth <= 1) { // don't go any further
            return new TEvent(label);
        }

        Tree<Relation<OWLObjectProperty>> children = duringTree.getChildren(event);

        if (children.childCount == 0) { // simple
            return new TEvent(label);
        }

        List<Relation<OWLObjectProperty>> subevents = children.children.stream()
                .map(t -> t.value)
                .flatMap(Streams::stream).toList();

        return new TParent(label, buildTimelineFromEvents(subevents, depth - 1));
    }

    private Timeline buildTimelineFromEvents(List<Relation<OWLObjectProperty>> events, int depth) {

        Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree =
                events.stream().collect(Collectors.toMap(ev -> ev, afterTree::getChildren));

        event2Tree.forEach((k, v) -> logger.info("          {} -> {}", k, v));

        Set<Relation<OWLObjectProperty>> roots = getRoots(event2Tree);

        if (roots.isEmpty()) {
            logger.error("No roots found");
        } else if (roots.size() == 1) {
            Relation<OWLObjectProperty> root = roots.iterator().next();

            logger.info("Single root found {}", root);

            return buildTimelineFrom(root, event2Tree, depth, false, false);
        }

        logger.info("Multiple roots found {}", roots.size());

        return new Timeline(SOMETIME_AFTER_REMOVE, List.of(new TConn(buildParallel(roots, event2Tree, depth), REMOVE_ME)), false, false);
    }

    private Timeline buildTimelineFrom(
            Relation<OWLObjectProperty> current,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth,
            boolean diverge,
            boolean converge) {
        List<TConn> chain = buildChainOnlyUsingGivenEventsFrom(new ArrayList<>(), current, event2Tree, depth);
        return new Timeline(diverge ? REMOVE_ME : SOMETIME_AFTER_REMOVE, chain, diverge, converge);
    }

    // TODO sometime...
    // TODO detect cycles?
    private List<TConn> buildChainOnlyUsingGivenEventsFrom(
            List<TConn> chain,
            Relation<OWLObjectProperty> current,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth) {

        chain.add(new TConn(buildNode(current, depth), prop(current)));

        Optional<Tree<Relation<OWLObjectProperty>>> treeNode = getSubtree(current.individual(), event2Tree);

        if (treeNode.isEmpty()) {
            logger.error("Not found in parent: {}. Render as faint", current);
            return chain;
        }
        else {
            Tree<Relation<OWLObjectProperty>> relationTree = treeNode.get();

            if (relationTree.childCount == 0) {
                logger.info("Last node in chain: {}", current);
                return chain;
            }

            if (relationTree.childCount == 1) {
                logger.info("Single path in chain: {}", current);
                return buildChainOnlyUsingGivenEventsFrom(chain, relationTree.children.get(0).value.get(0), event2Tree, depth);
            }

            logger.info("Divergent path in chain: {}", current);
            return buildParallel(chain, relationTree, event2Tree, depth);
        }
    }

    private TProp prop(Relation<OWLObjectProperty> current) {
        return props.get(current.property());
    }

    public List<TConn> buildParallel(
            List<TConn> chain,
            Tree<Relation<OWLObjectProperty>> treeNode,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth) {

        // TODO make chain immutable - return copy
        List<List<TConn>> divergentChains = treeNode.children.stream()
                .map(t -> buildChainOnlyUsingGivenEventsFrom(new ArrayList<>(), t.value.get(0), event2Tree, depth))
                .toList();

            buildConverging(chain, divergentChains);
        return chain;
    }

    private List<TConn> buildChainFrom(List<TConn> chain, Tree<Relation<OWLObjectProperty>> treeNode, int depth) {

        Relation<OWLObjectProperty> current = treeNode.value.get(0);

        chain.add(new TConn(buildNode(current, depth), prop(current)));

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

        chain.add(new TConn(divergentTimelines, REMOVE_ME));

        return chain;
    }

    private List<Timeline> buildParallel(
            Set<Relation<OWLObjectProperty>> roots,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth) {

        // TODO sort by longest first

        return roots.stream()
                .map(root -> buildTimelineFrom(root, event2Tree, depth, false, false))
                .sorted(Comparator.comparing(timeline -> -timeline.events().size()))
                .toList();
    }

    private Set<Relation<OWLObjectProperty>> getRoots(Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree) {
        // TODO also check for any which have roots NOT included in the parent
        return event2Tree.keySet().stream()
                .filter(e -> !isChildIn(e.individual(), event2Tree))
                .collect(Collectors.toSet());
    }


    private Optional<Tree<Relation<OWLObjectProperty>>> getSubtree(
            OWLNamedIndividual individual,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree) {
        return event2Tree.entrySet().stream().filter(e -> e.getKey().individual().equals(individual)).map(Map.Entry::getValue).findFirst();
    }

    private boolean isChildIn(OWLNamedIndividual event, Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree) {
        //TODO streams
        for (Tree<Relation<OWLObjectProperty>> parent : event2Tree.values()) {
            for (Tree<Relation<OWLObjectProperty>> child : parent.children) {
                for (Relation<OWLObjectProperty> r: child.value) {
                    if (r.individual().equals(event)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private String getLabel(OWLNamedIndividual event) {
        return sfp.getShortForm(event);
    }
}