package org.ontbrowser.www.url;

import javax.servlet.http.HttpServletRequest;
import java.net.URI;

public abstract class PagingURIScheme {
    protected final URI original;

    protected PagingURIScheme(HttpServletRequest request) {
        String queryString = request.getQueryString();
        this.original = URI.create("./" + (queryString != null ? "?" + queryString : ""));
    }

    // used in templates
    public abstract URI updateForPage(String componentName, int start, int pageSize);
}
