package org.coode.www.model;

public class ApplicationInfo {

    private final String name;
    private final String version;
    private final String url;

    public ApplicationInfo(String name, String version, String url) {
        this.name = name;
        this.version = version;
        this.url = url;
    }

    public String getName() {
        return name;
    }

    public String getVersion() {
        return version;
    }

    public String getUrl() {
        return url;
    }
}
