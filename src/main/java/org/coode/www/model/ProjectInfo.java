package org.coode.www.model;

public class ProjectInfo {

    private final String name;
    private final String url;
    private final String contact;
    private final String tagline;

    public ProjectInfo(String name, String contact, String url, String tagline) {
        this.name = name;
        this.contact = contact;
        this.url = url;
        this.tagline = tagline;
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
}
