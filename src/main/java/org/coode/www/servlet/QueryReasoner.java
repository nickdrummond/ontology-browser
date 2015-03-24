package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.owl.mngr.OWLClassExpressionParser;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.QueryType;
import org.coode.www.doclet.ReasonerResultsDoclet;
import org.coode.www.doclet.XMLResultsDoclet;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEntity;

import java.io.PrintWriter;
import java.net.URL;
import java.text.ParseException;
import java.util.*;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 17, 2007<br><br>
 */
public class QueryReasoner extends AbstractOntologyServerServlet {

    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params,
                                      OWLHTMLKit kit,
                                      URL pageURL) throws OntServerException {

        final Set<OWLEntity> results = getResults(params, kit);
        return new XMLResultsDoclet(results, kit);
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        final String query = params.get(OWLHTMLParam.query);
        final Set<OWLEntity> results = getResults(params, kit);
        return new ReasonerResultsDoclet(QueryType.valueOf(query), results, kit);
    }

    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params,
                                             OWLHTMLKit kit,
                                             URL pageURL) throws OntServerException {

        final String query = params.get(OWLHTMLParam.query);
        final String expression = params.get(OWLHTMLParam.expression);

        OWLDocPage page = new OWLDocPage(kit);
        page.addDoclet(new AbstractOWLDocDoclet(kit){

            protected void renderHeader(URL pageURL, PrintWriter out) {
                out.println("<h1>");
                out.println(expression);
                out.println("</h1>");
            }

            protected void renderFooter(URL pageURL, PrintWriter out) {
                // do nothing
            }

            public String getID() {
                return "doclet.expression.header";
            }
        });

        final Set<OWLEntity> results = getResults(params, kit);

        page.addDoclet(new ReasonerResultsDoclet(QueryType.valueOf(query), results, kit));
        return page;
    }



    private Set<OWLEntity> getResults(Map<OWLHTMLParam, String> params, OWLHTMLKit kit) throws OntServerException {
        final String expression = params.get(OWLHTMLParam.expression);
        final String syntax = params.get(OWLHTMLParam.syntax);
        final String query = params.get(OWLHTMLParam.query);

        OWLClassExpression classDescription = parse(expression, syntax, kit);

        return new HashSet<OWLEntity>(QueryType.valueOf(query).getResults(classDescription, kit));
    }

    public Map<OWLHTMLParam, Set<String>> getRequiredParams(OWLServer server) {
        Map<OWLHTMLParam, Set<String>> params = new HashMap<OWLHTMLParam,  Set<String>>();
        params.put(OWLHTMLParam.query, getQueryTypes());
        params.put(OWLHTMLParam.expression, Collections.singleton("<class expression>"));
        params.put(OWLHTMLParam.syntax, server.getSupportedSyntaxes());
        return params;
    }

    private Set<String> getQueryTypes(){
        Set<String> renderings = new HashSet<String>();
        for (QueryType type : QueryType.values()){
            renderings.add(type.toString());
        }
        return renderings;
    }

    private OWLClassExpression parse(String expression, String syntax, OWLHTMLKit kit) throws OntServerException {
        try {
            final OWLClassExpressionParser parser = kit.getOWLServer().getClassExpressionParser(syntax);
            return parser.parse(expression);
        }
        catch (ParseException e1) {
            throw new OntServerException(e1);
        }
    }
}
