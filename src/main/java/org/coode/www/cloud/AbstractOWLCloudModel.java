package org.coode.www.cloud;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Set;

public abstract class AbstractOWLCloudModel<O extends OWLObject> extends AbstractCloudModel<O> implements OWLCloudModel<O> {

    private Set<OWLOntology> ontologies;
    private ShortFormProvider renderer;

    protected AbstractOWLCloudModel(ShortFormProvider renderer) {
        this.renderer = renderer;
    }

    public final void setOntologies(Set<OWLOntology> onts) {
        ontologies = onts;
    }

    public final Set<OWLOntology> getOntologies() {
        return ontologies;
    }

    public String getRendering(O entity) {
        if (entity instanceof OWLEntity){
            return renderer.getShortForm((OWLEntity)entity);
        }
        else{
            return super.getRendering(entity);
        }
    }
}
