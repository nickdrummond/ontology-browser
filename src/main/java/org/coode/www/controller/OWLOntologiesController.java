package org.coode.www.controller;

import com.google.common.net.HttpHeaders;
import org.apache.commons.io.output.WriterOutputStream;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.With;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLOntologiesService;
import org.coode.www.service.hierarchy.OWLOntologyHierarchyService;
import org.coode.www.url.ComponentPagingURIScheme;
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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@Controller
@RequestMapping(value = "/ontologies")
public class OWLOntologiesController extends ApplicationController {

    @Autowired
    private OWLOntologiesService service;

    @Autowired
    private OntologyIRIShortFormProvider sfp;

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
        final HttpServletRequest request,
        final Model model
    ) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        Comparator<Tree<OWLOntology>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntologyHierarchyService hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), comparator);

        Tree<OWLOntology> ontologyTree = hierarchyService.getPrunedTree(ont);

        model.addAttribute("hierarchy", ontologyTree);

        getOntologyFragment(ontId, with, request, model);

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{ontId}/fragment")
    public String getOntologyFragment(
        @PathVariable final String ontId,
        @RequestParam(required = false) List<With> with,
        final HttpServletRequest request,
        final Model model
    ) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        String title = sfp.getShortForm(ont) + " (Ontology)";

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(ont);

        final IRI iri = ont.getOntologyID().getOntologyIRI().orElse(IRI.create("Anonymous"));

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();
        List<Characteristic> characteristics = service.getCharacteristics(ont, withOrEmpty, DEFAULT_PAGE_SIZE, kit);

        model.addAttribute("title", title);
        model.addAttribute("type", "Ontologies");
        model.addAttribute("iri", iri);
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("metrics", service.getMetrics(ont));
        model.addAttribute("showImportMetrics", !ont.getImports().isEmpty());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

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
