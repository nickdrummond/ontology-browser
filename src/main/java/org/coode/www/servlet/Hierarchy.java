package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.NodeDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.net.URL;
import java.util.Map;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Nov 12, 2010<br><br>
 */
public class Hierarchy extends AbstractOntologyServerServlet{

    @Override
    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        throw new NotImplementedException();
    }

    @Override
    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        throw new NotImplementedException();
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        HierarchyProvider hierarchyProvider = getHierarchyProvider(params.get(OWLHTMLParam.cls), kit);
        if (hierarchyProvider == null){
            throw new OntServerException("Cannot find a hierarchy for class " + params.get(OWLHTMLParam.cls));
        }
        OWLObject nodeObject = getNodeObject(params.get(OWLHTMLParam.parent), kit);
        if (nodeObject == null){
            throw new OntServerException("Cannot find an OWLObject with IRI " + params.get(OWLHTMLParam.parent));
        }
        NodeDoclet node =  new NodeDoclet(kit, nodeObject, hierarchyProvider);
        node.setUserObject(null);
        return node;
    }

    // TODO: how do we handle puns?
    private OWLObject getNodeObject(String s, OWLHTMLKit kit) {
        IRI iri = IRI.create(s);

        final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        if (iri.equals(df.getOWLThing().getIRI())){
            return df.getOWLThing();
        }
        else if (iri.equals(df.getTopDatatype().getIRI())){
            return df.getTopDatatype();
        }
        else if (iri.equals(df.getOWLTopDataProperty().getIRI())){
            return df.getOWLTopDataProperty();
        }
        else if (iri.equals(df.getOWLTopObjectProperty().getIRI())){
            return df.getOWLTopObjectProperty();
        }

        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            if (ont.containsClassInSignature(iri)){
                return df.getOWLClass(iri);
            }
            if (ont.containsObjectPropertyInSignature(iri)){
                return df.getOWLObjectProperty(iri);
            }
            if (ont.containsDataPropertyInSignature(iri)){
                return df.getOWLDataProperty(iri);
            }
            if (ont.containsAnnotationPropertyInSignature(iri)){
                return df.getOWLAnnotationProperty(iri);
            }
            if (ont.containsDatatypeInSignature(iri)){
                return df.getOWLDatatype(iri);
            }
            if (ont.containsIndividualInSignature(iri)){
                return df.getOWLNamedIndividual(iri);
            }
        }

        return kit.getOWLServer().getOntologyForIRI(iri);
    }

    private HierarchyProvider getHierarchyProvider(String classesStr, OWLHTMLKit kit) {
        String[] classes = classesStr.split(" ");
        for (String cls : classes){
            try {
                Class<OWLObject> c = (Class<OWLObject>)Class.forName("org.semanticweb.owlapi.model." + cls);
                HierarchyProvider<OWLObject> hp = kit.getOWLServer().getHierarchyProvider(c);
                if (hp != null){
                    return hp;
                }
            }
            catch (ClassNotFoundException e) {
                // do nothing - not a class
            }
            catch (ClassCastException e){
                // do nothing - not an OWLObject class
            }
        }
        return null;
    }
}
