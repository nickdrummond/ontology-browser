package org.ontbrowser.www.feature.editing;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ResponseStatusException;

import static org.springframework.http.HttpStatus.FORBIDDEN;

/**
 * Aspect to enforce @RequiresEditable on controller methods.
 */
@Aspect
@Component
@Order(1)
public class RequiresEditableAspect {

    private final OWLHTMLKit kit;

    @Autowired
    public RequiresEditableAspect(OWLHTMLKit kit) {
        this.kit = kit;
    }

    @Before("@annotation(org.ontbrowser.www.feature.editing.RequiresEditable)")
    public void checkEditable(JoinPoint joinPoint) {
        if (!kit.isEditable()) {
            throw new ResponseStatusException(FORBIDDEN, "Ontology is read-only");
        }
    }
}

