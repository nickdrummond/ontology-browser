package org.ontbrowser.www.renderer;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

public class OWLHTMLVisitorTest {

    public static Stream<Arguments> data() {
        return Stream.of(
                Arguments.of("http://example.org/thing#Name", "http://<wbr>example.org/<wbr>thing#<wbr><b>Name</b>"),
                Arguments.of("http://example.org/thing/", "http://<wbr>example.org/<wbr>thing/")
        );
    }

    private final OWLHTMLVisitor visitor = new OWLHTMLVisitor(
            mock(ShortFormProvider.class),
            mock(OntologyShortFormProvider.class),
            mock(IRIShortFormProvider.class),
            mock(URLScheme.class),
            mock(OWLOntology.class),
            mock(OWLEntityFinder.class)
    );

    @ParameterizedTest
    @MethodSource("data")
    public void test(String param1, String param2) {
        final StringWriter out = new StringWriter();
        visitor.setWriter(new PrintWriter(out));
        visitor.visit(IRI.create(param1));
        assertEquals(out.toString(), param2);
    }
}
