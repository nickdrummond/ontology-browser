package org.ontbrowser.www.url;

import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;

import static org.springframework.web.util.UriComponentsBuilder.fromUri;

public abstract class PagingURIScheme {
    protected final URI original;

    protected PagingURIScheme(String queryString) {
        this.original = URI.create(queryString != null ? "?" + queryString : "");
    }

    public UriComponentsBuilder builder() {
        return fromUri(original);
    }

    // used in templates
    public URI replacingParam(String paramName, String paramValue) {
        return builder()
                .replaceQueryParam(paramName, paramValue)
                .build(true) // state that all existing params are already encoded (do not double-encode)
                .toUri();
    }

    // used in templates
    public abstract URI updateForPage(String componentName, int start, int pageSize);
}
