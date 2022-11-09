package org.coode.www.controller;

import java.util.Optional;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLDataPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.OWLDataPropertyHierarchyService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;

@Controller
@RequestMapping(value="/dataproperties")
public class OWLDataPropertiesController extends ApplicationController {

    @Autowired
    private OWLDataPropertiesService service;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLDataProperties(final HttpServletRequest request) throws OntServerException {
        return redirect(request);
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLDataProperty(@PathVariable final String propertyId,
                                     final HttpServletRequest request) throws OntServerException, NotFoundException {
        return redirect(request);
    }

    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              final HttpServletRequest request) throws OntServerException, NotFoundException {
        return redirect(request);
    }
}
