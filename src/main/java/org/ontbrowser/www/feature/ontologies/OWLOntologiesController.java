package org.ontbrowser.www.feature.ontologies;

import com.google.common.net.HttpHeaders;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.io.output.WriterOutputStream;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.hierarchy.OWLOntologyHierarchyService;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDocumentFormat;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;

import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/ontologies")
public class OWLOntologiesController extends ApplicationController {

    private final OWLOntologiesService service;

    public OWLOntologiesController(OWLOntologiesService service) {
        this.service = service;
    }

    @GetMapping("/")
    public void getOntologiesOld(
            final HttpServletResponse response
    ) throws IOException {
        getOntologies(response);
    }

    @GetMapping()
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
            @RequestParam(required = false, defaultValue = "EXCLUDED") Imports imports,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        OWLOntologyHierarchyService hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), treeComparator());

        Tree<OWLOntology> ontologyTree = hierarchyService.getPrunedTree(ont);

        model.addAttribute("hierarchy", ontologyTree);

        getOntologyFragment(ontId, imports, with, model, request, response);

        return new ModelAndView("ontology");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{ontId}/fragment")
    public ModelAndView getOntologyFragment(
            @PathVariable final String ontId,
            @RequestParam(required = false, defaultValue = "EXCLUDED") Imports imports,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        var ontologySFP = kit.getOntologySFP();
        String title = ontologySFP.getShortForm(ont) + " (Ontology)";

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(ont);

        final IRI iri = ont.getOntologyID().getOntologyIRI().orElse(IRI.create("Anonymous"));

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();
        List<Characteristic> characteristics = service.getCharacteristics(ont, withOrEmpty, DEFAULT_PAGE_SIZE, kit);

        model.addAttribute("title", title);
        model.addAttribute("type", "Ontologies");
        model.addAttribute("iri", iri);
        model.addAttribute("ontId", ontId);
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("imports", imports);
        model.addAttribute("ontologiesSfp", ontologySFP);
        model.addAttribute("metrics", service.getMetrics(ont));
        model.addAttribute("showImportMetrics", imports == Imports.INCLUDED && !ont.getImports().isEmpty());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.name() + ": " + title);

        return new ModelAndView("ontologyfragment");
    }

    @GetMapping(value = "/{ontId}", produces = "application/rdf+xml")
    public void exportOntology(
            @PathVariable final String ontId,
            final HttpServletResponse response,
            final Writer writer
    ) {

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
