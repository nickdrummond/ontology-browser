package org.coode.www.controller;

import com.google.common.net.HttpHeaders;
import org.apache.commons.io.Charsets;
import org.apache.commons.io.output.WriterOutputStream;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.renderer.ElementRenderer;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLAxiomService;
import org.coode.www.service.OWLOntologiesService;
import org.coode.www.service.hierarchy.OWLOntologyHierarchyService;
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

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Comparator;
import java.util.Optional;
import java.util.Set;

import static org.coode.www.renderer.HighlightingHTMLRenderer.getHighlightRenderer;

@Controller
@RequestMapping(value = "/ontologies")
public class OWLOntologiesController extends ApplicationController {

    @Autowired
    private OWLOntologiesService service;

    @Autowired
    private OWLAxiomService axiomService;

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
    public String getOntology(@PathVariable final String ontId,
                              final Model model) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        Comparator<Tree<OWLOntology>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntologyHierarchyService hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), comparator);

        Tree<OWLOntology> ontologyTree = hierarchyService.getPrunedTree(ont);

        String title = sfp.getShortForm(ont) + " (Ontology)";

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(ont);

        final IRI iri = ont.getOntologyID().getOntologyIRI().orElse(IRI.create("Anonymous"));

        model.addAttribute("title", title);
        model.addAttribute("type", "Ontologies");
        model.addAttribute("iri", iri);
        model.addAttribute("hierarchy", ontologyTree);
        model.addAttribute("characteristics", service.getCharacteristics(ont, kit));
        model.addAttribute("metrics", service.getMetrics(ont));
        model.addAttribute("showImportMetrics", !ont.getImports().isEmpty());
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @GetMapping(value = "/{ontId}", produces = "application/rdf+xml")
    public void exportOntology(@PathVariable final String ontId,
                               final HttpServletResponse response,
                               final Writer writer) throws NotFoundException {

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


    @GetMapping(value = "/{ontId}/axioms/")
    public String axioms(
            final Model model,
            @PathVariable final String ontId,
            @RequestParam(required = false) Optional<String> search,
            @RequestParam(required = false, defaultValue = "true") boolean includeImports,
            @RequestParam(required = false, defaultValue = "20") int pageSize,
            @RequestParam(required = false, defaultValue = "1") int start
    ) throws NotFoundException {

        OWLOntology ont = service.getOntologyFor(ontId, kit);

        Set<OWLOntology> onts = includeImports ? ont.getImportsClosure() : Collections.singleton(ont);

        search.ifPresent(s -> {
            // Prevent injection attacks
            if (s.contains("<") || s.contains(">") || s.contains("%")) {
                throw new IllegalArgumentException("Search terms may be text only");
            }
        });

        ElementRenderer<OWLObject> owlRenderer = search
                .map(s -> getHighlightRenderer(s, rendererFactory.getRenderer(ont)))
                .orElse(rendererFactory.getRenderer(ont));

        Characteristic axioms = search
                .map(s -> axiomService.findAxioms(search.get(), onts, kit.getShortFormProvider(), start, pageSize))
                .orElse(axiomService.getAxioms(onts, start, pageSize));

        model.addAttribute("title", axioms.getName());
        model.addAttribute("axioms", axioms);
        model.addAttribute("mos", owlRenderer);

        return "axioms";
    }
}
