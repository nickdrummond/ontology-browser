package org.coode.www.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.NOT_FOUND)
public class NotFoundException extends Exception {

    public NotFoundException(String type, String id) {
        super("Could not find " + type + " = " + id);
    }

    public NotFoundException(String type) {
        super("Could not find any entities of type " + type);
    }
}
