package org.coode.www.controller;

import com.google.common.net.HttpHeaders;
import org.apache.commons.io.Charsets;
import org.apache.commons.io.output.WriterOutputStream;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.renderer.ElementRenderer;
import org.coode.www.renderer.Highlighter;
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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;

@Controller
@RequestMapping(value="/ontologies")
public class OWLOntologiesController extends ApplicationController {

    @Autowired
    private OWLOntologiesService service;

    @Autowired
    private OWLAxiomService axiomService;

    @Autowired
    private OntologyIRIShortFormProvider sfp;

    @RequestMapping(method=RequestMethod.GET)
    public String getOntologies() {

        OWLOntology rootOntology = kit.getRootOntology();

        String id = service.getIdFor(rootOntology);

        return "redirect:/ontologies/" + id;
    }

    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/{ontId}", method=RequestMethod.GET)
    public String getOntology(@PathVariable final String ontId,
                              final Model model) throws NotFoundException {
        OWLOntology owlOntology = service.getOntologyFor(ontId, kit);

        Comparator<Tree<OWLOntology>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntologyHierarchyService hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), comparator);

        Tree<OWLOntology> ontologyTree = hierarchyService.getPrunedTree(owlOntology);

        String title = sfp.getShortForm(owlOntology) + " (Ontology)";

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, owlOntology);

        final IRI iri = owlOntology.getOntologyID().getOntologyIRI().orElse(IRI.create("Anonymous"));

        model.addAttribute("title", title);
        model.addAttribute("type", "Ontologies");
        model.addAttribute("iri", iri);
        model.addAttribute("hierarchy", ontologyTree);
        model.addAttribute("characteristics", service.getCharacteristics(owlOntology, kit));
        model.addAttribute("metrics", service.getMetrics(owlOntology));
        model.addAttribute("showImportMetrics", !owlOntology.getImports().isEmpty());
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @RequestMapping(value="/{ontId}", method=RequestMethod.GET, produces="application/rdf+xml")
    public void exportOntology(@PathVariable final String ontId,
                               final HttpServletResponse response,
                               final Writer writer) throws NotFoundException {

        OWLOntology owlOntology = service.getOntologyFor(ontId, kit);

        try {
            OWLDocumentFormat format = new RDFXMLDocumentFormat();
            OutputStream out = new WriterOutputStream(writer, Charsets.toCharset("UTF-8"));
            response.addHeader(HttpHeaders.ACCEPT, "application/rdf+xml");
            kit.getOWLOntologyManager().saveOntology(owlOntology, format, out);
        }
        catch (OWLOntologyStorageException e) {
            throw new RuntimeException(e);
        }
    }


    @RequestMapping(value = "/{ontId}/axioms/")
    public String axioms(final Model model,
                         @PathVariable final String ontId,
                         @RequestParam(required = false) String search,
                         @RequestParam(required = false) String regex,
                         @RequestParam(required = false, defaultValue = "true") boolean includeImports) throws NotFoundException {

        OWLOntology owlOntology = service.getOntologyFor(ontId, kit);
        Set<OWLOntology> onts = new HashSet<>();
        if (includeImports) {
            onts.addAll(owlOntology.getImportsClosure());
        }
        else {
            onts.add(owlOntology);
        }

        ElementRenderer<OWLObject> owlRenderer;

        if (search != null || regex != null) {
            owlRenderer = new ElementRenderer<>() {

                private Highlighter highlighter = new Highlighter(search);

                @Override
                public String render(OWLObject object) {
                    return highlighter.highlight(new OWLHTMLRenderer(kit).render(object));
                }
            };
        }
        else {
            owlRenderer = new OWLHTMLRenderer(kit);
        }

        Set<OWLAxiom> axioms = (regex != null) ?
                axiomService.regexAxioms(regex, onts, kit.getShortFormProvider()) :
                axiomService.findAxioms(search, onts, kit.getShortFormProvider());

        model.addAttribute("title", axioms.size() + " axioms containing: " + search);
        model.addAttribute("axioms", axioms);
        model.addAttribute("mos", owlRenderer);

        return "axioms";
    }
}
