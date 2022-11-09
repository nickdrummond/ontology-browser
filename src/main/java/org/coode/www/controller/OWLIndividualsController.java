package org.coode.www.controller;

import java.util.List;
import java.util.Optional;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.Tree;
import org.coode.www.renderer.MediaRenderer;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.*;
import org.coode.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;
import java.util.Set;

@Controller
@RequestMapping(value="/individuals")
public class OWLIndividualsController extends ApplicationController {

    @Autowired
    private OWLIndividualsService service;

    @Autowired
    private OWLOntologiesService ontService;

    @Autowired
    private GeoService geoService;

    @Autowired
    private MediaService mediaService;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLIndividuals(final HttpServletRequest request) throws OntServerException, NotFoundException {
        return redirect(request);
    }


    @RequestMapping(value= "/{individualId}", method=RequestMethod.GET)
    public String getOWLIndividual(@PathVariable final String individualId,
                                   @RequestParam(required=false) final String ontId,
                                   final HttpServletRequest request,
                                   final Model model) throws OntServerException, NotFoundException {
        return redirect(request);
    }
}
