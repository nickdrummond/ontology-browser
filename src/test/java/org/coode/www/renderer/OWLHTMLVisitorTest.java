package org.coode.www.renderer;

import org.coode.html.url.URLScheme;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

@RunWith(Parameterized.class)
public class OWLHTMLVisitorTest {

    @Parameterized.Parameters(name = "{0}")
    public static Collection<String[]> data() {
        return Arrays.asList(new String[][]{
                {"With fragment", "http://example.org/thing#Name", "http://<wbr>example.org/<wbr>thing#<wbr><b>Name</b>"},
                {"Without fragment", "http://example.org/thing/", "http://<wbr>example.org/<wbr>thing/"}
        });
    }

    @Parameterized.Parameter
    public String name;

    @Parameterized.Parameter(1)
    public String fInput;

    @Parameterized.Parameter(2)
    public String fExpected;

    // TODO too many dependencies = smell
    private final OWLHTMLVisitor visitor = new OWLHTMLVisitor(
            mock(ShortFormProvider.class),
            mock(OntologyShortFormProvider.class),
            mock(URLScheme.class),
            Collections.emptySet(),
            mock(OWLOntology.class));

    @Test
    public void test() {
        final StringWriter out = new StringWriter();
        visitor.setWriter(new PrintWriter(out));
        visitor.visit(IRI.create(fInput));
        assertThat(out.toString(), equalTo(fExpected));
    }
}
