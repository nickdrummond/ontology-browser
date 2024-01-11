package org.coode.www.model.timeline;

import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class EventUtils {

    public static final Logger logger = LoggerFactory.getLogger(EventUtils.class);

    public static final TProp REMOVE_ME = new TProp("wrong");
    public static final TProp SOMETIME_AFTER_REMOVE = new TProp("wrong");

    public static List<TConn> buildConverging(List<TConn> chain, List<List<TConn>> divergentChains) {
        return buildConverging(chain, divergentChains, false);
    }

    public static List<TConn> buildConverging(
            List<TConn> chain,
            List<List<TConn>> divergentChains,
            boolean isConverging) {

        logger.info("Build Converging:");
        divergentChains.forEach(ch ->
                logger.info("chain: ${}", ch)
        );
        Set<TConn> lastElements = getLastElements(divergentChains);
        int differentEndingsCount = lastElements.size();

        if (differentEndingsCount == divergentChains.size()) {
            logger.info("All different ending - end ({})", isConverging ? "converging" : "diverging");
            // default all different - finish - either converging or not
            List<Timeline> timelines = divergentChains.stream()
                    .map(c -> new Timeline(REMOVE_ME, c, true, isConverging)).toList();
            chain.add(new TConn(timelines, REMOVE_ME));
        }
        else if (differentEndingsCount == 1) { // all have the same ending
            logger.info("All same ending - pull out the common");
            List<List<TConn>> trimmedLists = trim(divergentChains);
            chain.addAll(buildConverging(new ArrayList<>(), trimmedLists, true));
            chain.add(lastElements.iterator().next());
        }
        else {
            logger.info("Resolve different endings");
            // multiple matching sets
            // Will need to group them - ie change the order of divergingChains
            List<Timeline> timelines = new ArrayList<>();
            List<Timeline> parallelTimelines = new ArrayList<>();

            lastElements.forEach(element -> {
                List<List<TConn>> chains = getChainsMatchingLastElement(divergentChains, element);
                if (chains.size() == 1) {
                    logger.info("ending {} - {}", element, chains.get(0));
                    timelines.add(new Timeline(REMOVE_ME, chains.get(0), true, true));
                }
                else { // share a common end
                    List<List<TConn>> trimmedChains = trim(chains);
                    logger.info("ending {} - ", element);
                    trimmedChains.forEach(ch ->
                            logger.info("     trimmed: ${}", ch)
                    );

                    List<TConn> nested = buildConverging(new ArrayList<>(), trimmedChains, true);
                    nested.add(element);
                    parallelTimelines.add(new Timeline(REMOVE_ME, nested, true, true));
                }
            });

            // TODO Sort the single ones first
            timelines.addAll(parallelTimelines);

            chain.add(new TConn(timelines, REMOVE_ME));
        }
        return chain;
    }

    private static List<List<TConn>> trim(List<List<TConn>> chains) {
        return chains.stream().map(c -> c.subList(0, c.size() - 1)).toList();
    }

    private static List<List<TConn>> getChainsMatchingLastElement(List<List<TConn>> chains, TConn lastElement) {
        return chains.stream().filter(chain -> getLastElement(chain).equals(lastElement)).toList();
    }

    public static Set<TConn> getLastElements(List<List<TConn>> chains) {
        return new LinkedHashSet<>(chains.stream().map(EventUtils::getLastElement).toList()); // predictable ordering
    }

    public static TConn getLastElement(List<TConn> chain) {
        return chain.get(chain.size() - 1);
    }

//        Optional<Integer> year = getInteger(event, yearProp);
//        Optional<Integer> start = getInteger(event, startProp);
//        List<TParent> after = getRelationship(event, afterProp, depth);
//        List<String> participants = getParticipants(event, participantProp);

////        List<TParent> children = depth > 0 ? getChildren(event, depth - 1) : Collections.emptyList();
//        TParent e = null;//new TParent(label, new Timeline(null, children);//, start, year, after, participants, children);
//        cache.put(event, e);
//    }

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

    public static Optional<Integer> getInteger(
            OWLNamedIndividual event,
            OWLDataProperty prop,
            OWLOntology ont) {
        return ont.importsClosure()
                .flatMap(o -> o.dataPropertyAssertionAxioms(event))
                .filter(ax -> ax.getProperty().equals(prop))
                .map(OWLPropertyAssertionAxiom::getObject)
                .filter(OWLLiteral::isInteger)
                .map(lit -> Integer.parseInt(lit.getLiteral()))
                .findFirst();
    }
}
