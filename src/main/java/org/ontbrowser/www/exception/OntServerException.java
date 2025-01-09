package org.ontbrowser.www.exception;

public class OntServerException extends Exception {

    public OntServerException(){}

    public OntServerException(String s) {
        super(s);
    }

    public OntServerException(Exception e) {
        super(e);
    }

    public OntServerException(Throwable e) {
        super(e);
    }
}
