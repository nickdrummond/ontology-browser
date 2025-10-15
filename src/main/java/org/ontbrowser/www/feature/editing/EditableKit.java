package org.ontbrowser.www.feature.editing;

import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.io.OntologyLoader;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.impl.OWLHTMLKitInternals;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;

import java.util.List;

public class EditableKit extends RestartableKit {

    private static final Logger log = LoggerFactory.getLogger(EditableKit.class);

    private OWLHTMLKit editableDelegate;

    public EditableKit(
            OWLHTMLKit delegate,
            List<BeforeLoad> beforeLoad,
            ApplicationEventPublisher eventPublisher
    ) {
        super(delegate, beforeLoad, eventPublisher);
    }

    @Override
    protected OWLHTMLKit getDelegate() {
        if (EditModeContext.isEditMode()) {
            if (editableDelegate == null) {
                try {
                    // Create editable clone on first request
                    editableDelegate = createEditableClone();
                    return editableDelegate;
                } catch (OWLOntologyCreationException e) {
                    log.error("Failed to create editable clone. Cannot edit ontology", e);
                    EditModeContext.clear();
                }
            }
        }
        return super.getDelegate();
    }

    // Clone the original kit for editing
    private OWLHTMLKit createEditableClone() throws OWLOntologyCreationException {
        var config = getConfig();
        // TODO copy the original ontology instead of reloading from source
        var ont = new OntologyLoader().loadOntologies(config.root());
        return new OWLHTMLKitInternals(ont, config);
    }

    // Method to discard editable changes if needed
    public void resetEditableKit() {
        editableDelegate = null;
    }
}
