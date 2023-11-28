package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.hierarchy.RelationsHierarchyService;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class RelationsURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/relations";
    private final RelationsHierarchyService service;
    private String query = "";

    public RelationsURLScheme(OWLHTMLKit kit, RelationsHierarchyService service) {
        super(kit);
        this.service = service;
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject instanceof OWLNamedIndividual && service.treeContains((OWLNamedIndividual)owlObject)) {
            return ROOT_PATH
                    + "/onproperty/" + getIdForEntity(service.getProperty())
                    + "/withindividual/" + getIdForEntity((OWLNamedIndividual) owlObject)
                    + "/" + query;
        }
        else if (owlObject instanceof OWLObjectProperty) {
            return ROOT_PATH
                    + "/onproperty/" + getIdForEntity((OWLObjectProperty) owlObject)
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
