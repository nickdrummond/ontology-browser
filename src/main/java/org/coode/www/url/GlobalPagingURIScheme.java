package org.coode.www.url;

import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.servlet.http.HttpServletRequest;
import java.net.URI;

public class GlobalPagingURIScheme extends PagingURIScheme {
    public GlobalPagingURIScheme(HttpServletRequest request) {
        super(request);
    }

    public URI updateForPage(String componentName, int start, int pageSize) {
        return ServletUriComponentsBuilder.fromUri(original)
                .replaceQueryParam("start", start)
                .replaceQueryParam("pageSize", pageSize)
                .build().toUri();
    }
}
