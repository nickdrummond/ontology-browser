package org.coode.www.service.timeline;

import org.coode.www.model.timeline.TConn;
import org.coode.www.model.timeline.TProp;
import org.coode.www.model.timeline.Timeline;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

// TODO move to services
public class EventUtils {

    public static final Logger logger = LoggerFactory.getLogger(EventUtils.class);

    public static final TProp REMOVE_ME = new TProp("wrong");
    public static final TProp SOMETIME_AFTER_REMOVE = new TProp("sometimewrong");

    public static List<TConn> buildConverging(
            List<TConn> chain,
            List<List<TConn>> divergentChains) {
        return buildConverging(chain, divergentChains, true, false);
    }

    public static List<TConn> buildConverging(
            List<TConn> chain,
            List<List<TConn>> divergentChains,
            boolean isDiverging,
            boolean isConverging) {

        logger.info("Build Converging:");
        divergentChains.forEach(ch ->
                logger.info("chain: ${}", ch)
        );
        Set<TConn> lastElements = getLastElements(divergentChains);
        int differentEndingsCount = lastElements.size();

        if (differentEndingsCount == divergentChains.size()) { // all have different endings
            logger.info("All different ending - end ({})", isConverging ? "converging" : "diverging");
            // default all different - finish - either converging or not
            List<Timeline> timelines = divergentChains.stream()
                    .map(c -> new Timeline(c, REMOVE_ME, isDiverging, isConverging)).toList();
            chain.add(new TConn(REMOVE_ME, timelines));
        }
        else if (differentEndingsCount == 1) { // all have the same ending
            logger.info("All same ending - pull out the common");
            List<List<TConn>> trimmedLists = trim(divergentChains);
            chain.addAll(buildConverging(new ArrayList<>(), trimmedLists, isDiverging, true));
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
                    timelines.add(new Timeline(chains.get(0), REMOVE_ME, isDiverging, false));
                }
                else { // share a common end
                    List<List<TConn>> trimmedChains = trim(chains);
                    logger.info("ending {} - ", element);
                    trimmedChains.forEach(ch ->
                            logger.info("     trimmed: ${}", ch)
                    );

                    List<TConn> nested = buildConverging(new ArrayList<>(), trimmedChains, isDiverging, true);
                    nested.add(element);
                    parallelTimelines.add(new Timeline(nested, REMOVE_ME, isDiverging, true));
                }
            });

            // TODO Sort the single ones first
            timelines.addAll(parallelTimelines);

            chain.add(new TConn(REMOVE_ME, timelines));
        }
        return chain;
    }

    private static List<List<TConn>> trim(
            List<List<TConn>> chains) {
        return chains.stream().map(c -> c.subList(0, c.size() - 1)).toList();
    }

    private static List<List<TConn>> getChainsMatchingLastElement(
            List<List<TConn>> chains,
            TConn lastElement) {
        return chains.stream().filter(chain -> getLastElement(chain).equals(lastElement)).toList();
    }

    public static Set<TConn> getLastElements(
            List<List<TConn>> chains) {
        return new LinkedHashSet<>(chains.stream().map(EventUtils::getLastElement).toList()); // predictable ordering
    }

    public static TConn getLastElement(
            List<TConn> chain) {
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
