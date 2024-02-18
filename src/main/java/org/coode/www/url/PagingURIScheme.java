package org.coode.www.url;

import javax.servlet.http.HttpServletRequest;
import java.net.URI;

public abstract class PagingURIScheme {
    protected final URI original;

    protected PagingURIScheme(HttpServletRequest request) {
        this.original = URI.create(request.getRequestURI());
    }

    // used in templates
    public abstract URI updateForPage(String componentName, int start, int pageSize);
}
