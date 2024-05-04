package org.ontbrowser.www.url;

import org.ontbrowser.www.service.hierarchy.AbstractOWLHierarchyService;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

public class CommonRelationsURLScheme <T extends OWLEntity> extends RestURLScheme {

    public final String rootPath;
    private final AbstractOWLHierarchyService<OWLNamedIndividual> service;
    private final String propertyId;
    private final Class<? extends OWLEntity> propertyJavaClass;
    private String query = "";

    public CommonRelationsURLScheme(AbstractOWLHierarchyService<OWLNamedIndividual> service,
                                    String rootPath,
                                    T property) {
        this.service = service;
        this.rootPath = rootPath;
        this.propertyJavaClass = property.getClass();
        this.propertyId = getIdForEntity(property);
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject instanceof OWLNamedIndividual && service.treeContains((OWLNamedIndividual)owlObject)) {
            return rootPath + "/" + propertyId
                    + "/withindividual/" + getIdForEntity((OWLNamedIndividual) owlObject)
                    + "/" + query;
        }
        else if (propertyJavaClass.isAssignableFrom(owlObject.getClass())) {
            return rootPath + "/" +getIdForEntity((OWLEntity) owlObject)
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
