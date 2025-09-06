package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.hierarchy.AbstractOWLHierarchyService;
import org.ontbrowser.www.url.RestURLScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Arrays;

import static org.ontbrowser.www.url.EntityId.getIdForEntity;

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
    public String getURLForOWLObject(OWLObject owlObject, OWLOntology ontology) {
        if (service != null && owlObject instanceof OWLNamedIndividual ind && service.treeContains((OWLNamedIndividual)owlObject)) {
            return rootPath + "/" + propertyId + "/withindividual/" + getIdForEntity(ind) + query;
        }
        else if (propertyJavaClass.isAssignableFrom(owlObject.getClass())) {
            return rootPath + "/" + getIdForEntity((OWLEntity) owlObject) + query;
        }
        return super.getURLForOWLObject(owlObject, ontology);
    }

    // TODO Explicitly whitelist params to retain on links
    public URLScheme withQuery(String queryString) {
        if (queryString != null) {
            // DO NOT pass "with" params
            this.query = "?" + filter(queryString, "with");
        }
        return this;
    }

    private String filter(final String queryString, final String without) {
        var split = queryString.split("&");
        return Arrays.stream(split)
                .filter(s -> !s.startsWith(without))
                .reduce((a, b) -> a + "&" + b)
                .orElse("");
    }
}
