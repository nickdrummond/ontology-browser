package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.service.hierarchy.AbstractOWLHierarchyService;
import org.ontbrowser.www.url.RestURLScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

public class CommonRelationsURLScheme <T extends OWLEntity> extends RestURLScheme {

    public final String rootPath;
    private AbstractOWLHierarchyService<OWLNamedIndividual> service;
    private final String propertyId;
    private final Class<? extends OWLEntity> propertyJavaClass;
    private String query = "";

    public CommonRelationsURLScheme(String rootPath,
                                    T property) {
        this.rootPath = rootPath;
        this.propertyJavaClass = property.getClass();
        this.propertyId = getIdForEntity(property);
    }

    public CommonRelationsURLScheme withTree(AbstractOWLHierarchyService<OWLNamedIndividual> service) {
        this.service = service;
        return this;
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (service != null && owlObject instanceof OWLNamedIndividual ind && service.treeContains((OWLNamedIndividual)owlObject)) {
            return rootPath + "/" + propertyId + "/withindividual/" + getIdForEntity(ind) + query;
        }
        else if (propertyJavaClass.isAssignableFrom(owlObject.getClass())) {
            return rootPath + "/" + getIdForEntity((OWLEntity) owlObject);
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
