package org.coode.www.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.UNAUTHORIZED)
public class NotAuthorisedException extends Exception {

    public NotAuthorisedException() {
        super("Not authorised");
    }
}
