package org.ontbrowser.www.feature.editing;

import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;

/**
 * Controller advice that adds editingEnabled to the model if edit mode is on.
 */
@Profile("editing")
@ControllerAdvice
public class EditableControllerAdvice {
    @ModelAttribute("editingEnabled")
    public boolean addEditingEnabledAttribute() {
        return EditModeContext.isEditMode();
    }
}

