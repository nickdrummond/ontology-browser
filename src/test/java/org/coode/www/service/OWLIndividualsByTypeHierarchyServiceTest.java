package org.coode.www.service;

import org.coode.www.model.Tree;
import org.coode.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasoner;

import java.util.Comparator;

import static org.coode.www.service.Matchers.*;
import static org.junit.Assert.assertThat;

/**
 * Integration test using StructuralReasoner implementation.
 * To check that the behaviour of the reasoner and tree builder is correct.
 *
 */
public class OWLIndividualsByTypeHierarchyServiceTest {

    private OWLIndividualsByTypeHierarchyService service;

    private OWLDataFactory dataFactory;

    private OWLReasoner reasoner;

    private OWLOntology ontology;

    private OWLOntologyManager mngr;

    private Comparator<? super Tree<OWLEntity>> comparator;

    private OWLClass owlThing, a, a2;

    private OWLNamedIndividual i, i2, i3, i4;

    @Before
    public void setup() throws OWLOntologyCreationException {
        mngr = OWLManager.createOWLOntologyManager();
        dataFactory = mngr.getOWLDataFactory();
        owlThing = dataFactory.getOWLThing();

        ontology = mngr.createOntology();
        reasoner = new StructuralReasoner(ontology, new SimpleConfiguration(), BufferingMode.NON_BUFFERING);
        comparator = (o1, o2) -> o1.value.iterator().next().compareTo(o2.value.iterator().next());
        service = new OWLIndividualsByTypeHierarchyService(reasoner, comparator);

        a = cls("a");
        a2 = cls("a2");
        i = ind("i");
        i2 = ind("i2");
        i3 = ind("i3");
        i4 = ind("i4");
    }

    private void addAxiom(OWLAxiom axiom) {
        mngr.applyChange(new AddAxiom(ontology, axiom));
    }

    private OWLClass cls(String name) {
        return dataFactory.getOWLClass(IRI.create(name));
    }

    private OWLNamedIndividual ind(String name) {
        return dataFactory.getOWLNamedIndividual(IRI.create(name));
    }

    private void subs(OWLClass superCls, OWLClass... subClses) {
        for (OWLClass sub: subClses) {
            addAxiom(dataFactory.getOWLSubClassOfAxiom(sub, superCls));
        }
    }

    private void instances(OWLClass cls, OWLNamedIndividual... instances) {
        for (OWLNamedIndividual instance: instances) {
            addAxiom(dataFactory.getOWLClassAssertionAxiom(cls, instance));
        }
    }

    private void same(final OWLIndividual... inds) {
        addAxiom(dataFactory.getOWLSameIndividualAxiom(inds));
    }

    @Test
    public void owlThing() {
        Tree<OWLEntity> hierarchy = service.getPrunedTree(owlThing);

        assertThat(hierarchy, looksLike(
                t(owlThing)
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - i <- requested
     */
    @Test
    public void simplePath() {

        subs(owlThing, a);
        instances(a, i);

        Tree<OWLEntity> hierarchy = service.getPrunedTree(i);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a,
                                t(i)
                        )
                )
        ));
    }


    /**
     * owl:Thing
     * - i <- requested
     */
    @Test
    public void instanceOfThing() {

        instances(owlThing, i);

        Tree<OWLEntity> hierarchy = service.getPrunedTree(i);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(i)
                )
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - i <- requested
     *   - i2
     */
    @Test
    public void keepsSiblings() {

        subs(owlThing, a);
        instances(a, i, i2);

        Tree<OWLEntity> hierarchy = service.getPrunedTree(i);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a,
                                t(i),
                                t(i2)
                        )
                )
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - i  <- pruned
     *   - i2 <- pruned
     * - a2
     *   - i3
     *   - i4 <- requested
     */
    @Test
    public void prunes() {
        subs(owlThing, a, a2);
        instances(a, i, i2);
        instances(a2, i3, i4);

        Tree<OWLEntity> hierarchy = service.getPrunedTree(i4);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a),
                        t(a2,
                                t(i3),
                                t(i4)
                        )
                )
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - [i, i2]
     *   - [i3, i4] <- requested
     */
    @Test
    @Ignore // TODO re-enable
    public void sameAs() {
        subs(owlThing, a);
        instances(a, i, i3);
        same(i, i2);
        same(i3, i4);

        Tree<OWLEntity> hierarchy = service.getPrunedTree(i4);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a,
                                t(all(i, i2)),
                                t(all(i3, i4))
                        )
                )
        ));
    }
}
