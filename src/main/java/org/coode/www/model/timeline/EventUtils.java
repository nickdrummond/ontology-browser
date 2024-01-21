package org.coode.www.model.timeline;

import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class EventUtils {

    public static final Logger logger = LoggerFactory.getLogger(EventUtils.class);

    public static final TProp REMOVE_ME = new TProp("wrong");
    public static final TProp SOMETIME_AFTER_REMOVE = new TProp("sometimewrong");

    public static List<TConn<OWLNamedIndividual, OWLObjectProperty>> buildConverging(
            List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain,
            List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> divergentChains) {
        return buildConverging(chain, divergentChains, true, false);
    }

    public static List<TConn<OWLNamedIndividual, OWLObjectProperty>> buildConverging(
            List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain,
            List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> divergentChains,
            boolean isDiverging,
            boolean isConverging) {

        logger.info("Build Converging:");
        divergentChains.forEach(ch ->
                logger.info("chain: ${}", ch)
        );
        Set<TConn<OWLNamedIndividual, OWLObjectProperty>> lastElements = getLastElements(divergentChains);
        int differentEndingsCount = lastElements.size();

        if (differentEndingsCount == divergentChains.size()) { // all have different endings
            logger.info("All different ending - end ({})", isConverging ? "converging" : "diverging");
            // default all different - finish - either converging or not
            List<Timeline<OWLNamedIndividual, OWLObjectProperty>> timelines = divergentChains.stream()
                    .map(c -> new Timeline<>(c, REMOVE_ME, isDiverging, isConverging)).toList();
            chain.add(new TConn<>(REMOVE_ME, timelines));
        }
        else if (differentEndingsCount == 1) { // all have the same ending
            logger.info("All same ending - pull out the common");
            List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> trimmedLists = trim(divergentChains);
            chain.addAll(buildConverging(new ArrayList<>(), trimmedLists, isDiverging, true));
            chain.add(lastElements.iterator().next());
        }
        else {
            logger.info("Resolve different endings");
            // multiple matching sets
            // Will need to group them - ie change the order of divergingChains
            List<Timeline<OWLNamedIndividual, OWLObjectProperty>> timelines = new ArrayList<>();
            List<Timeline<OWLNamedIndividual, OWLObjectProperty>> parallelTimelines = new ArrayList<>();

            lastElements.forEach(element -> {
                List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> chains = getChainsMatchingLastElement(divergentChains, element);
                if (chains.size() == 1) {
                    logger.info("ending {} - {}", element, chains.get(0));
                    timelines.add(new Timeline<>(chains.get(0), REMOVE_ME, isDiverging, false));
                }
                else { // share a common end
                    List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> trimmedChains = trim(chains);
                    logger.info("ending {} - ", element);
                    trimmedChains.forEach(ch ->
                            logger.info("     trimmed: ${}", ch)
                    );

                    List<TConn<OWLNamedIndividual, OWLObjectProperty>> nested = buildConverging(new ArrayList<>(), trimmedChains, isDiverging, true);
                    nested.add(element);
                    parallelTimelines.add(new Timeline<>(nested, REMOVE_ME, isDiverging, true));
                }
            });

            // TODO Sort the single ones first
            timelines.addAll(parallelTimelines);

            chain.add(new TConn<>(REMOVE_ME, timelines));
        }
        return chain;
    }

    private static List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> trim(
            List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> chains) {
        return chains.stream().map(c -> c.subList(0, c.size() - 1)).toList();
    }

    private static List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> getChainsMatchingLastElement(
            List<List<TConn<OWLNamedIndividual, OWLObjectProperty>>> chains,
            TConn<OWLNamedIndividual, OWLObjectProperty> lastElement) {
        return chains.stream().filter(chain -> getLastElement(chain).equals(lastElement)).toList();
    }

    public static Set<TConn<OWLNamedIndividual, OWLObjectProperty>> getLastElements(
            List<List<TConn<org.semanticweb.owlapi.model.OWLNamedIndividual, org.semanticweb.owlapi.model.OWLObjectProperty>>> chains) {
        return new LinkedHashSet<>(chains.stream().map(EventUtils::getLastElement).toList()); // predictable ordering
    }

    public static TConn<OWLNamedIndividual, OWLObjectProperty> getLastElement(
            List<TConn<OWLNamedIndividual, OWLObjectProperty>> chain) {
        return chain.get(chain.size() - 1);
    }

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

    public static boolean filterByRelations(
            OWLNamedIndividual event,
            Set<OWLObjectPropertyExpression> relatedTo,
            OWLNamedIndividual object,
            OWLOntology ont) {
        return ont.importsClosure()
                .flatMap(o -> o.objectPropertyAssertionAxioms(event))
                .filter(ax -> relatedTo.contains(ax.getProperty()))
                .map(OWLPropertyAssertionAxiom::getObject)
                .anyMatch(o -> o.equals(object));
    }
}
