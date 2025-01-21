package org.ontbrowser.www.feature.ontologies;

import com.google.common.net.HttpHeaders;
import org.apache.commons.io.output.WriterOutputStream;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.OWLAxiomService;
import org.ontbrowser.www.service.hierarchy.OWLOntologyHierarchyService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.*;

import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/ontologies")
public class OWLOntologiesController extends ApplicationController {

    private final OWLOntologiesService service;
    private final OntologyIRIShortFormProvider sfp;
    private final OWLAxiomService axiomService;

    public OWLOntologiesController(
            @Autowired OWLOntologiesService service,
            @Autowired OntologyIRIShortFormProvider sfp,
            @Autowired OWLAxiomService axiomService) {
        this.service = service;
        this.sfp = sfp;
        this.axiomService = axiomService;
    }

    @GetMapping("/")
    public void getOntologies(
            final HttpServletResponse response
    ) throws IOException {

        OWLOntology rootOntology = kit.getRootOntology();

        String id = service.getIdFor(rootOntology);

        response.sendRedirect("/ontologies/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{ontId}")
    public ModelAndView getOntology(
        @PathVariable final String ontId,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        OWLOntologyHierarchyService hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), treeComparator());

        Tree<OWLOntology> ontologyTree = hierarchyService.getPrunedTree(ont);

        model.addAttribute("hierarchy", ontologyTree);

        getOntologyFragment(ontId, with, model, request, response);

        return new ModelAndView("owlentity");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{ontId}/fragment")
    public ModelAndView getOntologyFragment(
        @PathVariable final String ontId,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        String title = sfp.getShortForm(ont) + " (Ontology)";

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(ont);

        final IRI iri = ont.getOntologyID().getOntologyIRI().orElse(IRI.create("Anonymous"));

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();
        List<Characteristic> characteristics = service.getCharacteristics(ont, withOrEmpty, DEFAULT_PAGE_SIZE, kit);

        With axiomPaging = With.getOrDefault("axioms", withOrEmpty);
        Characteristic axioms = axiomService.getAxioms(ont, Imports.EXCLUDED, axiomPaging.start(), axiomPaging.pageSize());
        if (!axioms.getObjects().isEmpty()) {
            characteristics.add(axioms);
        }

        model.addAttribute("title", title);
        model.addAttribute("type", "Ontologies");
        model.addAttribute("iri", iri);
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("metrics", service.getMetrics(ont));
        model.addAttribute("showImportMetrics", !ont.getImports().isEmpty());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.name() + ": " + title);

        return new ModelAndView("owlentityfragment");

    }

    @GetMapping(value = "/{ontId}", produces = "application/rdf+xml")
    public void exportOntology(
        @PathVariable final String ontId,
        final HttpServletResponse response,
        final Writer writer
    ) throws NotFoundException {

        OWLOntology owlOntology = service.getOntologyFor(ontId, kit);

        try {
            OWLDocumentFormat format = new RDFXMLDocumentFormat();
            OutputStream out = new WriterOutputStream(writer, StandardCharsets.UTF_8);
            response.addHeader(HttpHeaders.ACCEPT, "application/rdf+xml");
            kit.getOWLOntologyManager().saveOntology(owlOntology, format, out);
        } catch (OWLOntologyStorageException e) {
            throw new RuntimeException(e);
        }
    }
}
