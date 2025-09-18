package org.ontbrowser.www.exception;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.ModelAndView;

@ControllerAdvice
public class EntityExceptionHandler {

    private final OWLHTMLKit kit; // Your service containing getRootOntology

    public EntityExceptionHandler(OWLHTMLKit kit) {
        this.kit = kit;
    }

    @ExceptionHandler(EntityNotFoundException.class)
    public ModelAndView handleEntityNotFound(EntityNotFoundException ex) {
        HttpServletRequest request =
                ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();

        if (!ex.getOntology().getOntologyID().equals(kit.getRootOntology().getOntologyID())) {
            return new ModelAndView(generateRedirectWithoutOntology(request));
        } else {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, ex.getMessage(), ex);
        }
    }


    private String generateRedirectWithoutOntology(HttpServletRequest request) {
        String query = "";

        if (request.getQueryString() != null) {
            var withoutOntId = request.getQueryString().replaceAll("ontId=[^&]*&?", "");
            if (!withoutOntId.isEmpty()) {
                query = "?" + withoutOntId;
            }
        }
        return "redirect:" + request.getRequestURI() + query;
    }
}