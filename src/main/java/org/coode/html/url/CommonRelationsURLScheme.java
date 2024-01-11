package org.coode.html.url;

import org.coode.www.service.hierarchy.AbstractHierarchyService;
import org.coode.www.service.hierarchy.Relation;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

public class CommonRelationsURLScheme <T extends OWLProperty> extends RestURLScheme {

    public final String rootPath;
    private final AbstractHierarchyService<Relation<T>> service;
    private final T property;
    private final String propertyId;
    private final Class<? extends OWLEntity> propertyJavaClass;
    private String query = "";

    public CommonRelationsURLScheme(AbstractHierarchyService<Relation<T>> service,
                                    String rootPath,
                                    T property) {
        this.service = service;
        this.rootPath = rootPath;
        this.property = property;
        this.propertyJavaClass = property.getClass();
        this.propertyId = getIdForEntity(property);
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if ((owlObject instanceof OWLNamedIndividual ind) && service.treeContains(new Relation<>(property, ind))) {
            return rootPath + "/" + propertyId
                    + "/withindividual/" + getIdForEntity(ind)
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
