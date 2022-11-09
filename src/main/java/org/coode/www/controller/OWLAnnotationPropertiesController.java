package org.coode.www.controller;

import java.util.Optional;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

@Controller
@RequestMapping(value="/annotationproperties")
public class OWLAnnotationPropertiesController extends ApplicationController {

    @Autowired
    private OWLAnnotationPropertiesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLAnnotationProperties(final HttpServletRequest request)
            throws OntServerException, NotFoundException {
        return redirect(request);
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLAnnotationProperty(@PathVariable final String propertyId,
                                           final HttpServletRequest request) throws OntServerException, NotFoundException {
        return redirect(request);
    }
}
