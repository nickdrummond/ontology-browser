package org.coode.www.service;

import com.google.common.collect.Sets;
import info.aduna.text.StringUtil;
import org.hamcrest.CustomTypeSafeMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.springframework.util.StringUtils;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.coode.www.service.OWLClassHierarchyService.Tree;

/**
 * Custom matchers
 */
public class Matchers {

    /**
     * Handy sugar for creating the comparison arrays
     * t(a, t(b), t(c))
     */
    public static Object[] t(Object... o) {
        return Arrays.stream(o)
                .map(i -> i instanceof OWLClass ? new OWLClassNode((OWLClass)i) : i)
                .collect(Collectors.toList()).toArray();
    }

    public static Node<OWLClass> all(OWLClass... clses) {
        return new OWLClassNode(Sets.newHashSet(clses));
    }

    public static Matcher<? super Tree<OWLClass>> looksLike(final Object[] expected) {

        return new CustomTypeSafeMatcher<Tree<OWLClass>>("A matching tree") {

            private Tree<OWLClass> parentNode;
            private Tree<OWLClass> actualNode;
            private Node expectedNode;

            @Override
            protected boolean matchesSafely(Tree<OWLClass> actual) {
                parentNode = actual;
                return matches(actual, expected);
            }

            @Override
            protected void describeMismatchSafely(Tree<OWLClass> item, Description mismatchDescription) {
                mismatchDescription
                        .appendText(parentNode != null ? parentNode.value.toString() : "no parent")
                        .appendText(" -> ")
                        .appendText(expectedNode != null ? expectedNode.toString() : "not expected")
                        .appendText(" was actually ")
                        .appendText(actualNode != null ? actualNode.value.toString() : "not there")
                        .appendText("\n\n")
                        .appendValue(item);
            }

            // Walk both actual and expected hierarchies
            private boolean childrenMatch(final Tree<OWLClass> actual, final Object[] expected) {
                for (int i=1; i<expected.length; i++) {
                    Object[] expectedChild = (Object[]) expected[i];

                    parentNode = actual;
                    if (actual.children.size() < i) {
                        expectedNode = (Node)expectedChild[0];
                        actualNode = null;
                        return false;
                    }
                    if (!matches(actual.children.get(i-1), expectedChild)) {
                        return false;
                    }
                }
                if (actual.children.size() > expected.length-1) {
                    parentNode = actual;
                    actualNode = actual.children.get(expected.length-1);
                    return false;
                }
                return true;
            }

            private boolean matches(final Tree<OWLClass> actual, final Object[] expected) {

                if (!actual.value.equals(expected[0])) {
                    expectedNode = (Node)expected[0];
                    actualNode = actual;
                    return false;
                }
                return childrenMatch(actual, expected);
            }
        };
    }
}
