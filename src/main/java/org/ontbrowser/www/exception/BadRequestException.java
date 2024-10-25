package org.ontbrowser.www.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

// TODO get rid of this and use Spring exceptions?
@ResponseStatus(value = HttpStatus.BAD_REQUEST)
public class BadRequestException extends Exception {

    public BadRequestException(String parameter, String value) {
        super("Bad parameter " + parameter + " = " + value);
    }

    public BadRequestException(String message) {
        super("Bad request: " + message);
    }
}
