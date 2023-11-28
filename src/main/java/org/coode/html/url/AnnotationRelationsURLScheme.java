package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.hierarchy.AnnotationsHierarchyService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

public class AnnotationRelationsURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/relations";
    private final AnnotationsHierarchyService service;
    private String query = "";

    public AnnotationRelationsURLScheme(OWLHTMLKit kit, AnnotationsHierarchyService service) {
        super(kit);
        this.service = service;
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject instanceof OWLNamedIndividual && service.treeContains((OWLNamedIndividual)owlObject)) {
            return ROOT_PATH
                    + "/onannotationproperty/" + getIdForEntity(service.getProperty())
                    + "/withindividual/" + getIdForEntity((OWLNamedIndividual) owlObject)
                    + "/" + query;
        }
        else if (owlObject instanceof OWLAnnotationProperty) {
            return ROOT_PATH
                    + "/onannotationproperty/" + getIdForEntity((OWLAnnotationProperty) owlObject)
                    + "/";
        }
        return super.getURLForOWLObject(owlObject);
    }

    public URLScheme withQuery(String queryString) {
        if (queryString != null) {
            this.query = "?" + queryString;
        }
        return this;
    }
}
