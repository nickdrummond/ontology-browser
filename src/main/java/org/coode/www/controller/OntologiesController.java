package org.coode.www.controller;

import com.google.common.base.Optional;
import com.google.common.net.HttpHeaders;
import org.apache.commons.io.output.WriterOutputStream;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.LoadOntology;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OntologiesService;
import org.coode.www.service.hierarchy.OWLOntologyHierarchyService;
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDocumentFormat;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import java.io.Writer;
import java.util.Comparator;

@Controller
@RequestMapping(value="/ontologies")
@SessionAttributes("kit")
public class OntologiesController extends ApplicationController {

    @Autowired
    private OntologiesService service;

    @Autowired
    private OntologyIRIShortFormProvider sfp;

    @RequestMapping(method=RequestMethod.GET)
    public String getOntologies(@ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        OWLOntology rootOntology = kit.getOWLServer().getRootOntology();

        String id = service.getIdFor(rootOntology);

        return "redirect:/ontologies/" + id;
    }

    @RequestMapping(value="/{ontId}", method=RequestMethod.GET)
    public String getOntology(@PathVariable final String ontId,
                              @ModelAttribute("kit") final OWLHTMLKit kit,
                              final Model model) throws OntServerException, NotFoundException {
        OWLOntology owlOntology = service.getOntologyFor(ontId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLOntology>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLOntologyHierarchyService hierarchyService = new OWLOntologyHierarchyService(owlServer.getRootOntology(), comparator);

        Tree<OWLOntology> ontologyTree = hierarchyService.getPrunedTree(owlOntology);

        String title = owlOntology.equals(owlServer.getRootOntology()) ?
                "All ontologies" :
                sfp.getShortForm(owlOntology) + " (Ontology)";

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.of(owlOntology));

        model.addAttribute("title", title);
        model.addAttribute("type", "Ontologies");
        model.addAttribute("iri", owlOntology.getOntologyID().getOntologyIRI().or(IRI.create("Anonymous")));
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("hierarchy", ontologyTree);
        model.addAttribute("characteristics", service.getCharacteristics(owlOntology, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @RequestMapping(value="/{ontId}", method=RequestMethod.GET, produces="application/rdf+xml")
    public void exportOntology(@PathVariable final String ontId,
                               @ModelAttribute("kit") final OWLHTMLKit kit,
                               final HttpServletResponse response,
                               final Writer writer) throws OntServerException, NotFoundException {

        OWLOntology owlOntology = service.getOntologyFor(ontId, kit);

        try {
            OWLDocumentFormat format = new RDFXMLDocumentFormat();
            WriterOutputStream out = new WriterOutputStream(writer);
            response.addHeader(HttpHeaders.ACCEPT, "application/rdf+xml");
            kit.getOWLServer().getOWLOntologyManager().saveOntology(owlOntology, format, out);
        }
        catch (OWLOntologyStorageException e) {
            throw new RuntimeException(e);
        }
    }

    @RequestMapping(method= RequestMethod.POST)
    public String loadOntology(@ModelAttribute final LoadOntology loadOntology,
                               @ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        String ontologyId = service.load(loadOntology.getUri(), loadOntology.isClear(), kit);

        if (loadOntology.getRedirect() != null) {
            return "redirect:" + loadOntology.getRedirect();
        }
        else {
            return "redirect:/ontologies/" + ontologyId;
        }
    }
}
