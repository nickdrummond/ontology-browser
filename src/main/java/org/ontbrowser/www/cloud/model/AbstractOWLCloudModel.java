package org.ontbrowser.www.cloud.model;

import com.google.common.collect.ImmutableSet;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Set;

public abstract class AbstractOWLCloudModel<O extends OWLEntity> extends AbstractCloudModel<O> {

    protected final Set<OWLOntology> ontologies;

    protected AbstractOWLCloudModel(final Set<OWLOntology> onts) {
        this.ontologies = ImmutableSet.copyOf(onts);
        reload();
    }
}
