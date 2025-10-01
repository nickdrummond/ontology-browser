package org.ontbrowser.www.feature.ontologies;

import com.google.common.net.HttpHeaders;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.io.output.WriterOutputStream;
import org.ontbrowser.www.feature.hierarchy.OWLOntologyHierarchyService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.ontbrowser.www.controller.Constants.DEFAULT_PAGE_SIZE;
import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/ontologies")
public class OWLOntologiesController {

    private final OWLHTMLKit kit;
    private final OWLOntologiesService service;

    public OWLOntologiesController(
            OWLHTMLKit kit,
            OWLOntologiesService service) {
        this.kit = kit;
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
        var rootOntology = kit.getRootOntology();
        var id = service.getIdFor(rootOntology);

        response.sendRedirect("/ontologies/" + id);
    }

    @GetMapping(value = "/{ontId}")
    public ModelAndView getOntology(
            @PathVariable final String ontId,
            @RequestParam(required = false, defaultValue = "EXCLUDED") Imports imports,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var ont = service.getOntologyFor(ontId, kit);
        var hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), treeComparator());
        var ontologyTree = hierarchyService.getPrunedTree(ont);
        model.addAttribute("hierarchy", ontologyTree);
        getOntologyFragment(ontId, imports, with, model, request);
        return new ModelAndView("ontology");
    }

    @GetMapping(value = "/{ontId}/fragment")
    public ModelAndView getOntologyFragment(
            @PathVariable final String ontId,
            @RequestParam(required = false, defaultValue = "EXCLUDED") Imports imports,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var ont = service.getOntologyFor(ontId, kit);

        var ontologySFP = kit.getOntologySFP();
        var title = ontologySFP.getShortForm(ont) + " (Ontology)";

        var mos = (OWLHTMLRenderer) model.getAttribute("mos");
        if (mos != null) {
            mos.withActiveObject(ont);
        }

        var iri = ont.getOntologyID().getOntologyIRI().orElse(IRI.create("Anonymous"));

        var characteristics = service.getCharacteristics(ont, with, DEFAULT_PAGE_SIZE, kit);

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
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request.getQueryString(), with));

        return new ModelAndView("ontologyfragment");
    }

    @GetMapping(value = "/{ontId}", produces = "application/rdf+xml")
    public void exportOntology(
            @PathVariable final String ontId,
            final HttpServletResponse response,
            final Writer writer
    ) {

        var owlOntology = service.getOntologyFor(ontId, kit);

        try {
            var format = new RDFXMLDocumentFormat();
            var out = new WriterOutputStream(writer, StandardCharsets.UTF_8);
            response.addHeader(HttpHeaders.ACCEPT, "application/rdf+xml");
            kit.getOWLOntologyManager().saveOntology(owlOntology, format, out);
        } catch (OWLOntologyStorageException e) {
            throw new RuntimeException(e);
        }
    }
}
