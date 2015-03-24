package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.owl.mngr.OWLServer;
import org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxEditorParser;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owlapi.expression.ParserException;
import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Map;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Nov 1, 2010<br><br>
 */
public class ParseMOS extends AbstractOntologyServerServlet {

    public ParseResult parse(String expression, OWLHTMLKit kit) throws ParseException {

        final OWLServer server = kit.getOWLServer();

        ManchesterOWLSyntaxEditorParser parser = new ManchesterOWLSyntaxEditorParser(server.getOWLOntologyManager().getOWLDataFactory(), expression);
        parser.setDefaultOntology(server.getActiveOntology());
        parser.setOWLEntityChecker(server.getOWLEntityChecker());

        try {
            parser.parseClassExpression();
            return new ParseResult(expression, "OK");
        }
        catch(ParserException e){
            throw new ParseException(expression, e.getMessage(), e.getStartPos(), e.getCurrentToken());
        }
    }

    @Override
    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Object response;
        try {
            response = parse(params.get(OWLHTMLParam.expression), kit);
        }
        catch (ParseException e) {
            response = e;
        }

        final Object resp = response;
        return new Doclet(){
            public void renderAll(URL pageURL, PrintWriter out) {
                out.print(resp);
            }
        };
    }

    @Override
    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        throw new OntServerException("HTML not supported");
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        throw new OntServerException("HTML not supported");
    }
}
