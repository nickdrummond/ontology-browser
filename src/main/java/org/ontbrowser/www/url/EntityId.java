package org.ontbrowser.www.url;

import org.semanticweb.owlapi.model.OWLEntity;

public class EntityId {

    public static String getIdForEntity(OWLEntity entity) {
        return String.valueOf(entity.getIRI().hashCode());
    }
}
