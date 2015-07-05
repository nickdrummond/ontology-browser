package org.coode.www.cloud;

import com.google.common.collect.ImmutableSet;
import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public abstract class AbstractOWLCloudModel<O extends OWLObject> extends AbstractCloudModel<O> implements OWLCloudModel<O> {

    protected final Set<OWLOntology> ontologies;
    private final ShortFormProvider renderer;
    protected final URLScheme urlScheme;

    protected AbstractOWLCloudModel(final Set<OWLOntology> onts,
                               final ShortFormProvider renderer,
                               final URLScheme urlScheme) {
        this.ontologies = ImmutableSet.copyOf(onts);
        this.renderer = renderer;
        this.urlScheme = urlScheme;
        reload();
    }

    public URL getURL(O entity) {
        return urlScheme.getURLForOWLObject(entity);
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
