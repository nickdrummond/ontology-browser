package org.ontbrowser.www.model;

import java.util.Arrays;
import java.util.List;

public class ProjectInfo {
    private String name;
    private String contact;
    private String url;
    private String tagline;
    private List<String> activeProfiles;

    public ProjectInfo(String name, String contact, String url, String tagline, String activeProfiles) {
        this(name, contact, url, tagline, Arrays.asList(activeProfiles.split(",")));
    }

    public ProjectInfo(String name, String contact, String url, String tagline, List<String> activeProfiles) {
        this.name = name;
        this.contact = contact;
        this.url = url;
        this.tagline = tagline;
        this.activeProfiles = activeProfiles;
    }

    public String name() { return name; }
    public void setName(String name) { this.name = name; }

    public String contact() { return contact; }
    public void setContact(String contact) { this.contact = contact; }

    public String url() { return url; }
    public void setUrl(String url) { this.url = url; }

    public String tagline() { return tagline; }
    public void setTagline(String tagline) { this.tagline = tagline; }

    public List<String> activeProfiles() { return activeProfiles; }
    public void setActiveProfiles(List<String> activeProfiles) { this.activeProfiles = activeProfiles; }
}
