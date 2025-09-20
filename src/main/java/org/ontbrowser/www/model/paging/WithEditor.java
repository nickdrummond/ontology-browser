package org.ontbrowser.www.model.paging;

import java.beans.PropertyEditorSupport;

public class WithEditor extends PropertyEditorSupport {

    @Override
    public void setAsText(String text) throws IllegalArgumentException {
        setValue(With.valueOf(text));
    }

    @Override
    public String getAsText() {
        return asText((With) getValue());
    }

    public static String asText(With with) {
        return with.toString();
    }
}
