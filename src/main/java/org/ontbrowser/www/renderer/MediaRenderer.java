package org.ontbrowser.www.renderer;

import org.ontbrowser.www.url.URLScheme;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.service.MediaService;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Set;

/**
 * A special version of the OWLHTMLRenderer that performs extra rendering tasks
 * - it renders IRIs with matching entities as entities
 * - it inlines images (and sounds)
 */
public class MediaRenderer extends OWLHTMLRenderer {

    private final OWLOntology ont;
    private final OWLEntityFinder entityFinder;

    private final MediaService mediaService;

    public MediaRenderer(final ShortFormProvider sfp,
                         final OntologyShortFormProvider ontSfp,
                         final URLScheme urlScheme,
                         final OWLOntology ont,
                         final OWLEntityFinder finder) {
        super(sfp, ontSfp, urlScheme, ont, finder);
        this.ont = ont;
        this.entityFinder = finder;
        this.mediaService = new MediaService();
    }

    @Override
    public String render(OWLObject obj) {
        StringBuilder sb = new StringBuilder();
        if (obj instanceof IRI){
            sb.append(handleIRI((IRI) obj));
        }
        else{
            sb.append( super.render(obj));
        }

        sb.append(tryInlineMedia(obj));

        return sb.toString();
    }

    private String tryInlineMedia(OWLObject obj) {
        IRI iri = null;
        if (obj instanceof OWLEntity){
            iri = ((OWLEntity)obj).getIRI();
        }
        else if (obj instanceof IRI){
            iri = (IRI)obj;
        }
        if (iri != null && mediaService.isImageURL(iri)){
            return "<img class=\"thumb\" src=\"" + iri + "\" height=\"100\" />";
        }
        return "";
    }

    // if an annotation value is an IRI with matching entities, write the entity links instead
    private String handleIRI(IRI value) {
        Set<? extends OWLEntity> entities = entityFinder.getOWLEntities(value, ont);
        if (entities.isEmpty()){
            return super.render(value);
        }
        else if (entities.size() == 1){
            return super.render(entities.iterator().next());
        }
        else{
            boolean started = false;
            StringBuilder sb = new StringBuilder();
            for (OWLEntity entity : entities){
                if (started){
                    sb.append(", ");
                }
                else{
                    started = true;
                }
                sb.append(super.render(entity));
                sb.append(" (");
                sb.append(entity.getEntityType().getPluralPrintName());
                sb.append(" )");
            }
            return sb.toString();
        }
    }
}