/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.html.util.HTMLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.util.OWLUtils;
import org.semanticweb.owlapi.model.*;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
@Deprecated
public class OntologyContentsDoclet extends AbstractOWLDocDoclet<OWLOntology> {

    public static final String ID = "doclet.contents";

    private String title;

    public OntologyContentsDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public void setTitle(String title){
        this.title = title;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        OWLOntology ont = getUserObject();

        renderBoxStart(getTitle(pageURL), getOntologyName(), out, pageURL);

        out.println("<ul>");

        for (NamedObjectType type : NamedObjectType.entitySubtypes()){
            int count = getOWLEntitiesFromOntology(type, ont).size();
            renderIndexLink(count, type, pageURL, out);
        }

        out.println("</ul>");
    }

    private Set<? extends OWLEntity> getOWLEntitiesFromOntology(NamedObjectType type, OWLOntology ont) {
        switch(type){
            case classes:
                Set<OWLClass> clses = new HashSet<OWLClass>(ont.getClassesInSignature());
                clses.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
                clses.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLNothing());
                return clses;
            case objectproperties:
                Set<OWLObjectProperty> ops = new HashSet<OWLObjectProperty>(ont.getObjectPropertiesInSignature());
                ops.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty());
                ops.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLBottomObjectProperty());
                return ops;
            case dataproperties:
                Set<OWLDataProperty> dps = new HashSet<OWLDataProperty>(ont.getDataPropertiesInSignature());
                dps.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLTopDataProperty());
                dps.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLBottomDataProperty());
                return dps;
            case annotationproperties: return ont.getAnnotationPropertiesInSignature();
            case individuals: return ont.getIndividualsInSignature();
            case datatypes:
                Set<OWLDatatype> dts = new HashSet<OWLDatatype>(ont.getDatatypesInSignature());
                dts.add(ont.getOWLOntologyManager().getOWLDataFactory().getTopDatatype());
                return dts;
            case entities: return ont.getSignature();
            default: throw new RuntimeException("Object type not known: " + type);
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        HTMLUtils.renderBoxEnd(getOntologyName(), out);
    }

    private String getTitle(URL pageURL) {
        if (title != null){
            return title;
        }
        else{
            // create link text
            URL ontURL = getOWLHTMLKit().getURLScheme().getURLForOWLObject(getUserObject());
            StringWriter writer = new StringWriter();
            PrintWriter out = new PrintWriter(writer);
            HTMLUtils.renderLink(getOntologyName(), ontURL,
                       null, pageURL, out);
            out.flush();
            return writer.getBuffer().toString();
        }
    }

    private String getOntologyName() {
        return getOWLHTMLKit().getOWLServer().getOntologyShortFormProvider().getShortForm(getUserObject());
    }

    private void renderIndexLink(int count, NamedObjectType type, URL pageURL, PrintWriter out) {
        if (count > 0){
            URL indexURL = getOWLHTMLKit().getURLScheme().getURLForOntologyIndex(getUserObject(), type);
            out.println("<li>");
            String label = type.getPluralRendering();
            HTMLUtils.renderLink(label, indexURL, null, pageURL, out);
            out.println(" (" + count + ")");
            out.println("</li>");
        }
    }

    public String getID() {
        return ID;
    }
}
