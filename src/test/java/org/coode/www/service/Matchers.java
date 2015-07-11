package org.coode.www.service;

import org.hamcrest.CustomTypeSafeMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.semanticweb.owlapi.model.OWLClass;
import static org.coode.www.service.OWLClassHierarchyService.Tree;

/**
 * Custom matchers
 */
public class Matchers {

    public static final String PREFIX = "http://example.com/";

    /**
     * Handy sugar for creating the comparison arrays
     * t("a", t("b"), t("c"))
     */
    public static Object[] t(Object... o) {
        return o;
    }

    public static Matcher<? super Tree<OWLClass>> looksLike(final Object[] expected) {

        return new CustomTypeSafeMatcher<Tree<OWLClass>>("A matching tree") {

            private Tree<OWLClass> parentNode;
            private Tree<OWLClass> actualNode;
            private String expectedNode = "";

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
                        .appendValue(expectedNode)
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
                        expectedNode = expectedChild[0].toString();
                        actualNode = null;
                        return false;
                    }
                    if (!matches(actual.children.get(i-1), expectedChild)) {
                        return false;
                    }
                }
                if (actual.children.size() > expected.length-1) {
                    parentNode = actual;
                    expectedNode = "expected no more nodes";
                    actualNode = actual.children.get(expected.length-1);
                    return false;
                }
                return true;
            }

            private boolean matches(final Tree<OWLClass> actual, final Object[] expected) {
                String nodeString = actual.value.getRepresentativeElement().toString();
                boolean nodeMatches = nodeString.equals(expected[0]) || nodeString.equals("<" + PREFIX + expected[0] + ">");
                if (!nodeMatches) {
                    expectedNode = expected[0].toString();
                    actualNode = actual;
                }
                return nodeMatches && childrenMatch(actual, expected);
            }
        };
    }
}
