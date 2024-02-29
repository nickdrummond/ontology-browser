package org.coode.www.url;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.servlet.http.HttpServletRequest;
import java.net.URI;

public class GlobalPagingURIScheme extends PagingURIScheme {

    private static final Logger log = LoggerFactory.getLogger(GlobalPagingURIScheme.class);

    public GlobalPagingURIScheme(HttpServletRequest request) {
        super(request);
    }

    public URI updateForPage(String componentName, int start, int pageSize) {
        URI uri = ServletUriComponentsBuilder.fromUri(original)
                .replaceQueryParam("start", start)
                .replaceQueryParam("pageSize", pageSize)
                .build().toUri();
        log.error("{} -> {} ", original, uri);
        return uri;
    }
}
