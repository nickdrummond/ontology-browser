package org.coode.www.cloud;

import com.google.common.collect.ImmutableSet;
import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.net.URL;
import java.util.Set;

public abstract class AbstractOWLCloudModel<O extends OWLEntity> extends AbstractCloudModel<O> {

    protected final Set<OWLOntology> ontologies;

    protected AbstractOWLCloudModel(final Set<OWLOntology> onts) {
        this.ontologies = ImmutableSet.copyOf(onts);
        reload();
    }
}
