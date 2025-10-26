package org.ontbrowser.www.feature.editing;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

/**
 * Context to hold edit mode state for the current thread.
 */
@Profile("editing")
@Component
public class EditModeContext {

    private static final ThreadLocal<Boolean> editMode = new ThreadLocal<>();

    public static void setEditMode(boolean isEditMode) {
        editMode.set(isEditMode);
    }

    public static boolean isEditMode() {
        return Boolean.TRUE.equals(editMode.get());
    }

    public static void clear() {
        editMode.remove();
    }
}
