package org.ontbrowser.www.interceptor;

import org.springframework.context.annotation.Profile;
import org.springframework.http.CacheControl;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.WebContentInterceptor;

import java.util.concurrent.TimeUnit;

@Component
@Profile("!dev")
public class CachingInterceptor extends WebContentInterceptor {

    public CachingInterceptor() {
        super();
        addCacheMapping(CacheControl.maxAge(1, TimeUnit.HOURS)
                .mustRevalidate(), "/**");
    }
}
