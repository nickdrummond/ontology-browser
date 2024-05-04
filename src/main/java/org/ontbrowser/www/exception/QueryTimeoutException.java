package org.ontbrowser.www.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.REQUEST_TIMEOUT)
public class QueryTimeoutException extends Exception {

    public QueryTimeoutException() {
        super("Timeout while performing query");
    }
}
