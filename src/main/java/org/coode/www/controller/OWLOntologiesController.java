package org.coode.www.controller;

import java.util.List;
import java.util.Optional;
import com.google.common.net.HttpHeaders;
import org.apache.commons.io.output.WriterOutputStream;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.LoadOntology;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLOntologiesService;
import org.coode.www.service.hierarchy.OWLOntologyHierarchyService;
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat;
import org.semanticweb.owlapi.metrics.OWLMetric;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.Writer;
import java.util.Comparator;

@Controller
@RequestMapping(value="/ontologies")
public class OWLOntologiesController extends ApplicationController {

    @Autowired
    private OWLOntologiesService service;

    @Autowired
    private OntologyIRIShortFormProvider sfp;

    @RequestMapping(method=RequestMethod.GET)
    public String getOntologies(final HttpServletRequest request) throws OntServerException {
        return redirect(request);
    }

    @RequestMapping(value="/{ontId}", method=RequestMethod.GET)
    public String getOntology(@PathVariable final String ontId,
                              final HttpServletRequest request) throws OntServerException, NotFoundException {
        return redirect(request);
    }

    @RequestMapping(value="/{ontId}", method=RequestMethod.GET, produces="application/rdf+xml")
    public String exportOntology(@PathVariable final String ontId,
                                 final HttpServletRequest request) throws OntServerException, NotFoundException {

        return redirect(request);
    }
}
