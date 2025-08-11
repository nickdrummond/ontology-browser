package org.ontbrowser.www.url;

import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class GlobalPagingURISchemeTest{

    @Test
    public void testGlobalPagingURIScheme() {
        var mockRequest = mock(HttpServletRequest.class);
        var current = "prefixes=&select=SELECT+%3Fs+%3Fp+%3Fo%0D%0AWHERE+%7B%0D%0A%3Fs+%3Fp+%3Fo+.%0D%0A%7D%0D%0A&pageSize=20&start=21";
        when(mockRequest.getQueryString()).thenReturn(current);

        var result = new GlobalPagingURIScheme(mockRequest)
                .updateForPage("test", 41, 20);

        assertEquals("?prefixes=&select=SELECT+%3Fs+%3Fp+%3Fo%0D%0AWHERE+%7B%0D%0A%3Fs+%3Fp+%3Fo+.%0D%0A%7D%0D%0A&start=41&pageSize=20", result.toString());
    }
}