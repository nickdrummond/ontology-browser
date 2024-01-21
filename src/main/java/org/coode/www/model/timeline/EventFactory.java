package org.coode.www.model.timeline;

import com.google.common.collect.Streams;
import org.coode.www.model.Tree;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.Relation;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.stream.Collectors;

import static org.coode.www.model.timeline.EventUtils.*;

public class EventFactory {

    public final Logger logger = LoggerFactory.getLogger(EventFactory.class);

    private final AbstractRelationsHierarchyService<OWLObjectProperty> duringTree;
    private final AbstractRelationsHierarchyService<OWLObjectProperty> afterTree;
    private final HashMap<OWLObjectProperty, TProp> props;

    public EventFactory(
            AbstractRelationsHierarchyService<OWLObjectProperty> duringTree,
            AbstractRelationsHierarchyService<OWLObjectProperty> afterTree) {
        this.duringTree = duringTree;
        this.afterTree = afterTree;
        this.props = new HashMap<>();
        afterTree.getProperties().forEach(p -> this.props.put(p, new TProp(p.getIRI().getFragment())));
        duringTree.getProperties().forEach(p -> this.props.put(p, new TProp(p.getIRI().getFragment())));
    }

    public Timeline<OWLNamedIndividual, OWLObjectProperty> buildTimeline(
            @Nonnull OWLNamedIndividual event,
            int depth) {
        // TODO Should be able to ask for the subtree without the property
        return buildTimelineFor(
                afterTree.getSubtree(new Relation<>(afterTree.getProperty(), event)),
                depth,
                false,
                false);
    }

    public Timeline<OWLNamedIndividual, OWLObjectProperty> buildTimelineFor(
            Tree<Relation<OWLObjectProperty>> eventTree,
            int depth,
            boolean diverge,
            boolean converge) {
        logger.info("buildTimelineFor: {}", eventTree);
        List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain = buildChainFrom(new ArrayList<>(), eventTree, depth);
        return new Timeline<>(chain, SOMETIME_AFTER_REMOVE, diverge, converge);
    }

    private TNode buildFaintNode(Relation<OWLObjectProperty> event) {
        return new TEvent<>(event.individual(), "faint");
    }

    private TNode buildNode(Relation<OWLObjectProperty> event, int depth) {

        if (depth <= 1) { // don't go any further
            return new TEvent<>(event.individual());
        }

        Tree<Relation<OWLObjectProperty>> children = duringTree.getChildren(event);

        if (children.childCount == 0) { // simple
            return new TEvent<>(event.individual());
        }

        List<Relation<OWLObjectProperty>> subevents = children.children.stream()
                .map(t -> t.value)
                .flatMap(Streams::stream).toList();

        return new TParent<>(event.individual(), buildTimelineFromEvents(subevents, depth - 1));
    }

    private Timeline<OWLNamedIndividual, OWLObjectProperty> buildTimelineFromEvents(
            List<Relation<OWLObjectProperty>> events, int depth) {

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

        return new Timeline<>(List.of(
                new TConn<>(REMOVE_ME, buildParallel(roots, event2Tree, depth))),
                SOMETIME_AFTER_REMOVE, false, false);
    }

    private Timeline<OWLNamedIndividual, OWLObjectProperty> buildTimelineFrom(
            Relation<OWLObjectProperty> current,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth,
            boolean diverge,
            boolean converge) {
        List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain =
                buildChainOnlyUsingGivenEventsFrom(new ArrayList<>(), current, event2Tree, depth);
        return new Timeline<>(chain, converge ? REMOVE_ME : SOMETIME_AFTER_REMOVE, diverge, converge);
    }

    // TODO detect cycles?
    private List<TConn<OWLNamedIndividual, OWLObjectProperty>> buildChainOnlyUsingGivenEventsFrom(
            List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain,
            Relation<OWLObjectProperty> current,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth) {

        Optional<Tree<Relation<OWLObjectProperty>>> treeNode = getSubtree(current.individual(), event2Tree);

        if (treeNode.isEmpty()) {
            logger.info("Not found in parent: {}. Rendering as faint", current);
            chain.add(new TConn<>(
                    prop(current).withMeta("faint"),
                    buildFaintNode(current)));
            // TODO stop the chain here?
            return chain;
        }
        else {
            chain.add(new TConn<>(prop(current), buildNode(current, depth)));

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

    public List<TConn<OWLNamedIndividual, OWLObjectProperty>> buildParallel(
            List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain,
            Tree<Relation<OWLObjectProperty>> treeNode,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth) {

        // TODO make chain immutable - return copy
        List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> divergentChains = treeNode.children.stream()
                .map(t -> buildChainOnlyUsingGivenEventsFrom(new ArrayList<>(), t.value.get(0), event2Tree, depth))
                .toList();

        buildConverging(chain, divergentChains);
        return chain;
    }

    private List<TConn<OWLNamedIndividual, OWLObjectProperty>> buildChainFrom(
            List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain,
            Tree<Relation<OWLObjectProperty>> treeNode,
            int depth) {

        Relation<OWLObjectProperty> current = treeNode.value.get(0);

        chain.add(new TConn<>(prop(current), buildNode(current, depth)));

        if (treeNode.children.isEmpty()) {
            logger.info("Last node in chain: {}", current);
            return chain;
        }

        if (treeNode.children.size() == 1) {
            logger.info("Single path in chain: {}", current);
            return buildChainFrom(chain, treeNode.children.get(0), depth);
        }

        // TODO what if diverging timelines on different properties?
        logger.info("Divergent path in chain: {}", current);
        List<Timeline<OWLNamedIndividual, OWLObjectProperty>> divergentTimelines = treeNode.children.stream()
                .map(t -> buildTimelineFor(t, depth, true, false))
                .toList();

        chain.add(new TConn<>(REMOVE_ME, divergentTimelines));

        return chain;
    }

    private List<Timeline<OWLNamedIndividual, OWLObjectProperty>> buildParallel(
            Set<Relation<OWLObjectProperty>> roots,
            Map<Relation<OWLObjectProperty>, Tree<Relation<OWLObjectProperty>>> event2Tree,
            int depth) {

        // TODO sort by longest first

// TODO options 1: treat them as independent timelines - allows us to "stretch" parallels
//        return roots.stream()
//                .map(root -> buildTimelineFrom(root, event2Tree, depth, false, false))
//                .sorted(Comparator.comparing(timeline -> -timeline.events().size()))
//                .toList();

// TODO  option 2 But this allows converging timelines - however, everything is a parallel so the "stretching" cannot be applied
        List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> chains = roots.stream()
                .map(root -> buildChainOnlyUsingGivenEventsFrom(new ArrayList<>(), root, event2Tree, depth))
//                .sorted(Comparator.comparing(timeline -> -timeline.events().size()))
                .toList();


        // TODO some will converge, but still need a list of timelines as some won't
        List<TConn<OWLNamedIndividual, OWLObjectProperty>> converging = buildConverging(new ArrayList<>(), chains, false, false);
        return List.of(new Timeline<>(converging, REMOVE_ME, false, false));
        // end of option 2
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
}