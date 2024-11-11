package org.ontbrowser.www.model;

import java.util.Arrays;
import java.util.List;

public record ProjectInfo (String name, String contact, String url, String tagline, List<String> activeProfiles){

    public ProjectInfo(String name, String contact, String url, String tagline, String activeProfiles) {
        this(name, contact, url, tagline, Arrays.asList(activeProfiles.split(",")));
    }
}
