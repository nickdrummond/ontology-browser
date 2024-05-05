package org.ontbrowser.www.controller;

import com.google.common.net.HttpHeaders;
import org.apache.commons.io.output.WriterOutputStream;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.OWLAxiomService;
import org.ontbrowser.www.service.OWLOntologiesService;
import org.ontbrowser.www.service.hierarchy.OWLOntologyHierarchyService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.*;

@Controller
@RequestMapping(value = "/ontologies")
public class OWLOntologiesController extends ApplicationController {

    @Autowired
    private OWLOntologiesService service;

    @Autowired
    private OntologyIRIShortFormProvider sfp;

    @Autowired
    private OWLAxiomService axiomService;

    @GetMapping("/")
    public String getOntologies() {

        OWLOntology rootOntology = kit.getRootOntology();

        String id = service.getIdFor(rootOntology);

        return "redirect:/ontologies/" + id;
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{ontId}")
    public String getOntology(
        @PathVariable final String ontId,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        Comparator<Tree<OWLOntology>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntologyHierarchyService hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), comparator);

        Tree<OWLOntology> ontologyTree = hierarchyService.getPrunedTree(ont);

        model.addAttribute("hierarchy", ontologyTree);

        getOntologyFragment(ontId, with, model, request, response);

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{ontId}/fragment")
    public String getOntologyFragment(
        @PathVariable final String ontId,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        String title = sfp.getShortForm(ont) + " (Ontology)";

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(ont);

        final IRI iri = ont.getOntologyID().getOntologyIRI().orElse(IRI.create("Anonymous"));

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();
        List<Characteristic> characteristics = service.getCharacteristics(ont, withOrEmpty, DEFAULT_PAGE_SIZE, kit);

        With axiomPaging = With.getOrDefault("axioms", withOrEmpty);
        Characteristic axioms = axiomService.getAxioms(Collections.singleton(ont), axiomPaging.start(), axiomPaging.pageSize());
        if (!axioms.getObjects().isEmpty()) {
            characteristics.add(axioms);
        }

        model.addAttribute("title", title);
        model.addAttribute("type", "Ontologies");
        model.addAttribute("iri", iri);
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("metrics", service.getMetrics(ont));
        model.addAttribute("showImportMetrics", !ont.getImports().isEmpty());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.getName() + ": " + title);

        return "owlentityfragment";
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
