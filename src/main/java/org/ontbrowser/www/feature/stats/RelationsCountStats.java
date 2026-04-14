package org.ontbrowser.www.feature.stats;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Counts the cumulative number of ObjectPropertyAssertion axioms for a property
 * and all of its descendants in the property hierarchy.
 *
 * <h2>In-memory strategy</h2>
 * At construction time we make a <em>single pass</em> over all
 * {@code ObjectPropertyAssertion} axioms to build a {@code directCounts} map
 * (O(A) where A = number of assertions). Cumulative totals are then computed
 * lazily on demand via a depth-first traversal of the <em>direct</em>
 * sub-property hierarchy, memoised in {@code cumulativeCounts}.
 *
 * <h2>In-DB alternative</h2>
 * When the ontology is stored in the OWLDB2 schema the same result can be
 * obtained in two SQL queries (or one if a CTE is used), exploiting the
 * pre-built indexes that are not available to the in-memory OWLAPI:
 *
 * <pre>{@code
 * -- 1. Direct count for a single property (uses entity_axioms index):
 * SELECT COUNT(*)
 *   FROM entity_axioms ea
 *   JOIN axioms a ON ea.axiom_id = a.id
 *  WHERE ea.entity_iri   = :propertyIri
 *    AND ea.entity_type  = 'ObjectProperty'
 *    AND a.axiom_type    = 'ObjectPropertyAssertion'
 *    AND ea.ontology_id  = ANY(:ontologyIds);
 *
 * -- 2. Cumulative count using the object_property_hierarchy table:
 * WITH RECURSIVE descendants AS (
 *   SELECT child_iri
 *     FROM object_property_hierarchy
 *    WHERE ontology_id = ANY(:ontologyIds) AND parent_iri = :propertyIri
 *   UNION
 *   SELECT h.child_iri
 *     FROM object_property_hierarchy h
 *     JOIN descendants d ON h.parent_iri = d.child_iri
 *      AND h.ontology_id = ANY(:ontologyIds)
 * )
 * SELECT COUNT(*)
 *   FROM entity_axioms ea
 *   JOIN axioms a ON ea.axiom_id = a.id
 *  WHERE ea.entity_type = 'ObjectProperty'
 *    AND a.axiom_type   = 'ObjectPropertyAssertion'
 *    AND ea.ontology_id = ANY(:ontologyIds)
 *    AND ea.entity_iri  IN (SELECT :propertyIri UNION ALL SELECT child_iri FROM descendants);
 *
 * -- 3. Or bulk-load all direct counts at once, then roll up in Java:
 * SELECT ea.entity_iri, COUNT(*) AS direct_count
 *   FROM entity_axioms ea
 *   JOIN axioms a ON ea.axiom_id = a.id
 *  WHERE ea.entity_type = 'ObjectProperty'
 *    AND a.axiom_type   = 'ObjectPropertyAssertion'
 *    AND ea.ontology_id = ANY(:ontologyIds)
 *  GROUP BY ea.entity_iri;
 * }</pre>
 *
 * Option 3 is the most efficient when stats for all properties are needed
 * simultaneously (as in a property hierarchy tree), because it replaces N
 * individual queries with a single aggregation query and then reuses the
 * in-memory hierarchy traversal above for roll-up.
 */
class RelationsCountStats implements Stats<OWLObjectProperty> {

    public static final String NAME = "relationsCount";

    /**
     * Direct assertion counts per named property, built in O(A) at construction.
     * Properties with zero assertions are absent (use {@code getOrDefault}).
     */
    private final Map<OWLObjectProperty, Integer> directCounts;

    /**
     * Lazily populated cumulative counts (direct + all descendants).
     * Populated during {@link #getStats} traversal.
     */
    private final Map<OWLObjectProperty, Integer> cumulativeCounts = new HashMap<>();

    private final OWLReasoner reasoner;

    public RelationsCountStats(OWLReasoner reasoner) {
        this.reasoner = reasoner;

        // Single pass: count ObjectPropertyAssertions grouped by property.
        this.directCounts = reasoner.getRootOntology()
                .axioms(AxiomType.OBJECT_PROPERTY_ASSERTION, Imports.INCLUDED)
                .filter(ax -> ax.getProperty().isNamed())
                .collect(Collectors.groupingBy(
                        ax -> ax.getProperty().asOWLObjectProperty(),
                        Collectors.collectingAndThen(Collectors.counting(), Long::intValue)
                ));
    }

    @Override
    public int getStats(OWLObjectProperty target) {
        return computeCumulative(target);
    }

    @Override
    public String getName() {
        return NAME;
    }

    /**
     * Depth-first memoised accumulation using only <em>direct</em>
     * sub-properties ({@code direct=true}) to avoid counting any descendant
     * more than once.
     *
     * <p>A placeholder is written before recursing so that cycles formed by
     * equivalent properties (which the structural reasoner can surface as
     * self-edges) do not cause a stack overflow.
     */
    private int computeCumulative(OWLObjectProperty prop) {
        if (cumulativeCounts.containsKey(prop)) {
            return cumulativeCounts.get(prop);
        }

        int direct = directCounts.getOrDefault(prop, 0);
        // Write a placeholder first to break any cycle from equivalent properties.
        cumulativeCounts.put(prop, direct);

        int fromDirectSubs = reasoner.getSubObjectProperties(prop, true)
                .entities()
                .filter(OWLObjectPropertyExpression::isNamed)
                .map(OWLObjectPropertyExpression::asOWLObjectProperty)
                .filter(sub -> !sub.equals(prop))   // guard against self-loops
                .mapToInt(this::computeCumulative)
                .sum();

        int total = direct + fromDirectSubs;
        cumulativeCounts.put(prop, total);
        return total;
    }
}
