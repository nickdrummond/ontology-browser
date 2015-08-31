package org.coode.www.renderer;


import com.google.common.base.Optional;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.MediaService;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Set;

/**
 * A special version of the OWLHTMLRenderer that performs extra rendering tasks
 * - it renders IRIs with matching entities as entities
 * - it inlines images (and sounds)
 */
public class MediaRenderer extends OWLHTMLRenderer {

    private final OWLEntityFinder entityFinder;

    // TODO inject
    private final MediaService mediaService;

    private boolean inlineMedia = true;

    public MediaRenderer(OWLHTMLKit kit, Optional<? extends OWLObject> activeObject) {
        super(kit, activeObject);
        this.entityFinder = kit.getFinder();
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

        if (inlineMedia){
            sb.append(tryInlineMedia(obj));
        }

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
        if (iri != null){
            if (mediaService.isImageURL(iri)){
                return "<img class=\"thumb\" src=\"" + iri + "\" height=\"100\" />";
            }
            else{
                // TODO: make a play button
//                if (URLUtils.isSoundURL(iri)){
//                    out.print("<EMBED src=\"");
//                    out.print(iri);
//                    out.println("\" autostart=\"true\" hidden=\"true\"/>");
//                }
            }
        }
        return "";
    }

    // if an annotation value is an IRI with matching entities, write the entity links instead
    private String handleIRI(IRI value) {
        Set<? extends OWLEntity> entities = entityFinder.getOWLEntities(value, NamedObjectType.entities);
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
                sb.append(NamedObjectType.getType(entity).getPluralRendering());
                sb.append(" )");
            }
            return sb.toString();
        }
    }
}