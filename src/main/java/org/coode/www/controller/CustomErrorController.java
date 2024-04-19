package org.coode.www.controller;

import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseStatus;

import javax.servlet.http.HttpServletRequest;
import java.util.Set;

// see https://spring.io/blog/2013/11/01/exception-handling-in-spring-mvc
@Controller
public class CustomErrorController extends ApplicationController {

    private static final Logger log = LoggerFactory.getLogger(CustomErrorController.class);

    @GetMapping("/error")
    public String handleError(
            HttpServletRequest httpRequest,
            Exception e,
            Model model) throws Exception {

        // If the exception is annotated with @ResponseStatus rethrow it and let
        // the framework handle it - like the OrderNotFoundException example
        // at the start of this post.
        // AnnotationUtils is a Spring Framework utility class.
        if (AnnotationUtils.findAnnotation(e.getClass(), ResponseStatus.class) != null) {
            log.warn("Rethrowing", e);
            throw e;
        }

        log.error(e.getMessage(), e);

        Set<OWLOntology> ontologies = kit.getOntologies();

        OWLOntology ont = kit.getActiveOntology();

        Object errorCode = httpRequest.getAttribute("javax.servlet.error.status_code");
        if (errorCode instanceof Integer code) {
            model.addAttribute("errorCode", code);
        }
        else {
            log.warn("Unknown error code {} : {}", errorCode.getClass(), errorCode);
            model.addAttribute("errorCode", 0);
        }

        // TODO needed??
        model.addAttribute("activeOntology", ont);
        model.addAttribute("ontologies", ontologies);

        return "error";
    }
}