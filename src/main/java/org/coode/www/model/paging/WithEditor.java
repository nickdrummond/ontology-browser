package org.coode.www.model.paging;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.beans.PropertyEditorSupport;

public class WithEditor extends PropertyEditorSupport {
    private static final Logger log = LoggerFactory.getLogger(WithEditor.class);

    @Override
    public void setAsText(String text) throws IllegalArgumentException {
        String[] parts = text.split(" ");
        int start = Math.max(1, Integer.parseInt(parts[1]));
        int pageSize = Math.max(1, Integer.parseInt(parts[2]));
        setValue(new With(parts[0], start, pageSize));
    }

    @Override
    public String getAsText() {
        return asText((With) getValue());
    }

    public static String asText(With with) {
        return with.characteristicName() + " " + with.start() + " " + with.pageSize();
    }
}
