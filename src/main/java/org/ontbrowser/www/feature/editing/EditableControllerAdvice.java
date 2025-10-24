package org.ontbrowser.www.feature.editing;

import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;

/**
 * Controller advice that adds editing information to the model when edit mode is active.
 */
@Profile("editing")
@ControllerAdvice
public class EditableControllerAdvice {

    /**
     * Adds an "editingEnabled" attribute to the model if edit mode is active.
     *
     * @return boolean value indicating if edit mode is enabled
     */
    @ModelAttribute("editingEnabled")
    public boolean addEditingEnabledAttribute() {
        return EditModeContext.isEditMode();
    }
}
