package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.feature.hierarchy.OWLClassHierarchyService;
import org.ontbrowser.www.feature.stats.Stats;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.ontbrowser.www.feature.graph.GraphURLScheme;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.ui.Model;
import org.springframework.web.servlet.ModelAndView;

import java.util.*;

import static org.ontbrowser.www.model.Tree.treeComparator;

public class CommonFragments {

    private final OWLHTMLKit kit;
    private final ProjectInfo projectInfo;
    private final ReasonerService reasonerService;

    public CommonFragments(final OWLHTMLKit kit, final ProjectInfo projectInfo, final ReasonerService reasonerService) {
        this.kit = kit;
        this.projectInfo = projectInfo;
        this.reasonerService = reasonerService;
    }

    @SuppressWarnings("SameReturnValue")
    public ModelAndView getOWLClassFragment(
            final OWLClassesService service,
            final OWLClass owlClass,
            final OWLOntology ont,
            final OWLHTMLRenderer owlRenderer,
            List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        ShortFormProvider sfp = kit.getShortFormProvider();

        String entityName = sfp.getShortForm(owlClass);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = service.getCharacteristics(
                owlClass, ont, kit.getComparator(),
                withOrEmpty,
                ApplicationController.DEFAULT_PAGE_SIZE);

        Set<OWLClass> namedSuperclasses = service.getNamedTypes(owlClass, ont);

        String supers = String.join(", ", namedSuperclasses.stream().map(sfp::getShortForm).toList());

        String title = entityName + (supers.isEmpty() ? "" : " (" + supers + ")");

        model.addAttribute("title", title);
        model.addAttribute("type", "Classes");
        model.addAttribute("iri", owlClass.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.name() + ": " + title);
        return new ModelAndView("owlentityfragment");
    }

    public ModelAndView getClassChildren(
            final OWLClass cls,
            final OWLReasoner r,
            final OWLHTMLRenderer owlRenderer,
            final Stats stats,
            final Model model) {

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, treeComparator());
        Tree<OWLClass> prunedTree = hierarchyService.getChildren(cls);

        model.addAttribute("t", prunedTree);
        model.addAttribute("stats", stats);
        model.addAttribute("statsName", stats.getName());
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("base::children");
    }


    public ModelAndView getPropertyChildren(
            final OWLClass cls,
            final OWLReasoner r,
            final OWLHTMLRenderer owlRenderer,
            final Stats stats,
            final Model model) {

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, treeComparator());
        Tree<OWLClass> prunedTree = hierarchyService.getChildren(cls);

        model.addAttribute("stats", stats);
        model.addAttribute("statsName", stats.getName());
        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("base::children");
    }


    public ModelAndView getOWLIndividualFragment(
            final OWLIndividualsService service,
            final OWLNamedIndividual owlIndividual,
            final boolean inferred,
            List<With> with,
            final OWLOntology ont,
            final OWLHTMLRenderer owlHtmlRenderer,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        ShortFormProvider sfp = kit.getShortFormProvider();

        String entityName = sfp.getShortForm(owlIndividual);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = new ArrayList<>(
                service.getCharacteristics(owlIndividual, ont, kit.getComparator(), withOrEmpty, ApplicationController.DEFAULT_PAGE_SIZE));

        if (inferred) {
            OWLReasoner reasoner = reasonerService.getReasoner();
            characteristics.addAll(service.getInferredCharacteristics(owlIndividual, reasoner));
        }

        Set<OWLClass> namedTypes = service.getNamedTypes(owlIndividual, ont);

        String types = String.join(", ", namedTypes.stream().map(sfp::getShortForm).toList());

        String title = entityName + (types.isEmpty() ? "" : " (" + types + ")");

        model.addAttribute("title", title);
        model.addAttribute("type", "Individuals");
        model.addAttribute("iri", owlIndividual.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));
        model.addAttribute("mos", owlHtmlRenderer);

        if (projectInfo.activeProfiles().contains("graph")) {
            var mos = new MOSStringRenderer(kit.getFinder(), ont);
            model.addAttribute("graphLink", new GraphURLScheme(mos).getURLForOWLObject(owlIndividual));
        }

        response.addHeader("title", projectInfo.name() + ": " + title);

        return new ModelAndView("owlentityfragment");
    }
}
