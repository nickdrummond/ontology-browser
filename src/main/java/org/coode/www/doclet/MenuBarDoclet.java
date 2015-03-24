/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.HTMLUtils;
import org.coode.owl.mngr.ServerConstants;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.coode.owl.mngr.ServerProperty;
import org.coode.owl.mngr.impl.LabelShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class MenuBarDoclet extends AbstractOWLDocDoclet {

    private static final Logger logger = Logger.getLogger(MenuBarDoclet.class);

    public static final String ID = "doclet.menubar";

    private static final String RENDERER_NAME = "renderLabels";

    private static final String RENDERER_FORM = "rendererForm";


    public MenuBarDoclet(OWLHTMLKit kit) {
        super(kit);
        addDoclet(createSearch(kit));
        addDoclet(new SignoutDoclet(kit));
        addDoclet(createActiveOntDoclet(kit));
    }

    private HTMLDoclet  createSearch(OWLHTMLKit kit) {
        AutocompleteDoclet searchboxDoclet = new AutocompleteDoclet(kit, "find", true);
        searchboxDoclet.setParamName("input");
        searchboxDoclet.setSubmitName("find");
        searchboxDoclet.setSubmitURL(kit.getURLScheme().getURLForRelativePage("find/"));
        searchboxDoclet.setTarget(OWLHTMLConstants.LinkTarget.content);
        return searchboxDoclet;
    }

    private HTMLDoclet createActiveOntDoclet(final OWLHTMLKit kit) {
        final ServerPropertiesAdapter<ServerProperty> serverProps = kit.getOWLServer().getProperties();
        return new OptionSelectorDoclet(kit,
                                        ServerProperty.optionActiveOnt.name(),
                                        serverProps.get(ServerProperty.optionActiveOnt),
                                        serverProps.getAllowedValues(ServerProperty.optionActiveOnt)){

            @Override
            public String getID() {
                return "activeOnt";
            }

            @Override
            protected String renderValue(String value) {
                if (value.equals(kit.getOWLServer().getRootOntology().getOntologyID().getDefaultDocumentIRI().toString())){
                    return ServerConstants.ROOT_ONTOLOGY_RENDERING;
                }
                return super.renderValue(value);
            }
        };
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {

        out.println("\n\n<div id='menu'>");

        out.println("<a style='display: none;' href='#content'>skip to content</a> ");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {

        renderOptions(pageURL, out);

        out.println("</div> <!-- menu -->\n");
    }

    private void renderOptions(URL pageURL, PrintWriter out) {
        out.println("\n<div id='options'>");

        final URL optionsURL = getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML);

        HTMLUtils.renderLink("Options", optionsURL, OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);

        out.print("| <form id='");
        out.print(RENDERER_FORM);
        out.println("' style='display: inline;'>");

        out.print("<label for='");
        out.print(RENDERER_NAME);
        out.println("'>Render labels</label>");

        out.print("<input type='checkbox' name='");
        out.print(RENDERER_NAME);
        out.print("' id=\"");
        out.print(RENDERER_NAME);
        out.print("\"");
        if (isRenderLabels()){
            out.print(" checked='checked'");
        }
        out.println(" />");

        out.println("</form>");

        out.println("</div> <!-- options -->\n");
    }

    private boolean isRenderLabels() {
        final ShortFormProvider sfp = getOWLHTMLKit().getOWLServer().getShortFormProvider();
        return sfp.getClass().equals(LabelShortFormProvider.class);
    }

    public String getID() {
        return ID;
    }
}
