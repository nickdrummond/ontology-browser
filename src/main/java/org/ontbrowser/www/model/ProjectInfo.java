package org.ontbrowser.www.model;

import java.util.Arrays;
import java.util.List;

public class ProjectInfo {

    private final String name;
    private final String url;
    private final String contact;
    private final String tagline;
    private final List<String> activeProfiles;

    public ProjectInfo(String name, String contact, String url, String tagline, String activeProfiles) {
        this.name = name;
        this.contact = contact;
        this.url = url;
        this.tagline = tagline;
        this.activeProfiles = Arrays.asList(activeProfiles.split(","));
    }

    public String getName() {
        return name;
    }

    public String getContact() {
        return contact;
    }

    public String getUrl() {
        return url;
    }

    public String getTagline() { return tagline; }

    public List<String> getActiveProfiles() {
        return activeProfiles;
    }
}
