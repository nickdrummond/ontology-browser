package org.ontbrowser.www.service;

import com.google.common.collect.Iterators;
import com.google.common.collect.Sets;
import org.ontbrowser.www.model.Tree;
import org.hamcrest.CustomTypeSafeMatcher;
import org.hamcrest.Description;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNode;

import java.util.Arrays;

/**
 * Custom matchers
 */
public class Matchers {

    /**
     * Handy sugar for creating the comparison arrays
     * t(a, t(b), t(c))
     */
    @SuppressWarnings("GrazieInspection")
    public static Object[] t(Object... o) {
        return Arrays.stream(o).map(Matchers::toNode).toArray();
    }

    private static Object toNode(Object o) {
        if (o instanceof OWLClass) {
            return new OWLClassNode((OWLClass)o);
        }
        else if (o instanceof OWLNamedIndividual) {
            return new OWLNamedIndividualNode((OWLNamedIndividual)o);
        }
        return o;
    }

    public static Node<OWLClass> all(OWLClass... clses) {
        return new OWLClassNode(Sets.newHashSet(clses));
    }

    public static Node<OWLNamedIndividual> all(OWLNamedIndividual... inds) {
        return new OWLNamedIndividualNode(Sets.newHashSet(inds));
    }

    public static CustomTypeSafeMatcher<Tree<? extends OWLEntity>> looksLike(final Object[] expected) {

        return new CustomTypeSafeMatcher<>("A matching tree") {

            private Tree<? extends OWLEntity> parentNode;
            private Tree<? extends OWLEntity> actualNode;
            private Iterable<? extends OWLEntity> expectedNode;

            @Override
            protected boolean matchesSafely(Tree<? extends OWLEntity> actual) {
                parentNode = actual;
                return matches(actual, expected);
            }

            @Override
            protected void describeMismatchSafely(Tree<? extends OWLEntity> item, Description mismatchDescription) {
                mismatchDescription
                        .appendText(parentNode != null ? parentNode.value.toString() : "no parent")
                        .appendText(" -> ")
                        .appendText(expectedNode != null ? expectedNode.toString() : "not expected")
                        .appendText(" was actually ")
                        .appendText(actualNode != null ? actualNode.value.getClass() + " " + actualNode.value : "not there")
                        .appendText("\n\n")
                        .appendValue(item);
            }

            // Walk both actual and expected hierarchies
            private boolean childrenMatch(final Tree<? extends OWLEntity> actual, final Object[] expected) {
                for (int i = 1; i < expected.length; i++) {
                    Object[] expectedChild = (Object[]) expected[i];

                    parentNode = actual;
                    if (actual.children.size() < i) {
                        expectedNode = getNodeFrom(expectedChild[0]);
                        actualNode = null;
                        return false;
                    }
                    if (!matches(actual.children.get(i - 1), expectedChild)) {
                        return false;
                    }
                }
                if (actual.children.size() > expected.length - 1) {
                    parentNode = actual;
                    actualNode = actual.children.get(expected.length - 1);
                    return false;
                }
                return true;
            }

            private boolean matches(final Tree<? extends OWLEntity> actual, final Object[] expected) {
                Iterable<? extends OWLEntity> nodeFromExpected = getNodeFrom(expected[0]);
                if (!Iterators.elementsEqual(actual.value.iterator(), nodeFromExpected.iterator())) {
                    expectedNode = nodeFromExpected;
                    actualNode = actual;
                    return false;
                }
                return childrenMatch(actual, expected);
            }

            private Iterable<? extends OWLEntity> getNodeFrom(Object o) {
                if (o instanceof OWLClassNode) {
                    return (OWLClassNode) o;
                }
                else if (o instanceof OWLNamedIndividualNode) {
                    return (OWLNamedIndividualNode) o;
                }
                throw new RuntimeException("Unsupported tree node: " + o.getClass());
            }
        };
    }
}
