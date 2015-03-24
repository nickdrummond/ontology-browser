package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.renderer.OWLHTMLRenderer;
import org.coode.owl.util.OWLUtils;
import org.semanticweb.owlapi.model.*;

import java.io.PrintWriter;
import java.net.URL;
import java.util.*;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 4, 2010<br><br>
 */
public class OWLIndividualSummaryDoclet extends AbstractOWLDocDoclet<OWLNamedIndividual> {

    public OWLIndividualSummaryDoclet(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new OWLEntityTitleDoclet<OWLNamedIndividual>(kit));
//        addDoclet(new AnnotationsDoclet<OWLNamedIndividual>(kit));
        addDoclet(new TypesDoclet(kit));
        addDoclet(new SameAsDoclet(kit));
        addDoclet(new DifferentFromDoclet(kit));
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.write("<div class='summary'>");
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.write("</div> <!-- summary -->");
    }

    public String getID() {
        return "doclet.summary.individual";
    }

    @Override
    public void setUserObject(OWLNamedIndividual individual) {
        super.setUserObject(individual);
        if (individual != null){
            createAnnotationDoclets(individual);
            createPropertyAssertionDoclets(individual);
            addDoclet(new UsageDoclet<OWLNamedIndividual>(getOWLHTMLKit()));
        }
    }

    private void createPropertyAssertionDoclets(OWLNamedIndividual individual) {

        final Map<OWLPropertyExpression, Set<OWLPropertyAssertionObject>> assertedProps =
                OWLUtils.getAssertedPropertyMap(individual, getOWLHTMLKit().getOWLServer().getActiveOntologies());

        final List<OWLPropertyExpression> orderedProps = new ArrayList<OWLPropertyExpression>(assertedProps.keySet());

        Collections.sort(orderedProps, getOWLHTMLKit().getOWLObjectComparator());
        
        for (final OWLPropertyExpression prop : orderedProps){
            addDoclet(new AbstractOWLElementsDoclet<OWLNamedIndividual, OWLPropertyAssertionObject>(prop.toString(), ElementsDoclet.Format.list, getOWLHTMLKit()){

                protected Collection<OWLPropertyAssertionObject> getAssertedElements(Set<OWLOntology> ontologies) {
                    return assertedProps.get(prop);
                }

                // TODO: inferred elements
//                    @Override
//                    protected Collection<OWLPropertyAssertionObject> getInferredElements(Set<OWLOntology> ontologies) {
//
//                    }

                protected void renderBoxStart(String name, String id, PrintWriter out, URL pageURL) {
                    name = new OWLHTMLRenderer(getOWLHTMLKit()).render(prop, pageURL);
                    super.renderBoxStart(name, id, out, pageURL);
                }
            });
        }
    }

    private void createAnnotationDoclets(OWLNamedIndividual individual) {

        final Map<OWLAnnotationProperty, Set<OWLAnnotationValue>> assertedProps =
                OWLUtils.getAnnotationPropertyMap(individual, getOWLHTMLKit().getOWLServer().getActiveOntologies());

        final List<OWLAnnotationProperty> orderedProps = new ArrayList<OWLAnnotationProperty>(assertedProps.keySet());

        Collections.sort(orderedProps, getOWLHTMLKit().getOWLObjectComparator());

        for (final OWLAnnotationProperty prop : orderedProps){
            addDoclet(new AbstractOWLElementsDoclet<OWLNamedIndividual, OWLAnnotationValue>(prop.toString(), ElementsDoclet.Format.list, getOWLHTMLKit()){

                protected Collection<OWLAnnotationValue> getAssertedElements(Set<OWLOntology> ontologies) {
                    return assertedProps.get(prop);
                }

                protected void renderBoxStart(String name, String id, PrintWriter out, URL pageURL) {
                    name = new OWLHTMLRenderer(getOWLHTMLKit()).render(prop, pageURL);
                    super.renderBoxStart(name, id, out, pageURL);
                }
            });
        }
    }
}
