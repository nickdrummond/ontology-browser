package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.hierarchy.RelationsHierarchyService;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

public class RelationsURLScheme extends RelationPropertyURLScheme {

    private final RelationsHierarchyService service;

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
                    + "/?inverse=" + service.isInverse();
        }
        return super.getURLForOWLObject(owlObject);
    }
}
