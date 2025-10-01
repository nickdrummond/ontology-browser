package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.feature.graph.GraphURLScheme;
import org.ontbrowser.www.feature.hierarchy.OWLClassHierarchyService;
import org.ontbrowser.www.feature.stats.Stats;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.stereotype.Service;
import org.springframework.ui.Model;
import org.springframework.web.servlet.ModelAndView;

import java.util.ArrayList;
import java.util.List;

import static org.ontbrowser.www.controller.Constants.DEFAULT_PAGE_SIZE;
import static org.ontbrowser.www.model.Tree.treeComparator;

@Service
public class CommonFragments {

    private final OWLHTMLKit kit;
    private final ProjectInfo projectInfo;
    private final ReasonerService reasonerService;

    public CommonFragments(
            OWLHTMLKit kit,
            ProjectInfo projectInfo,
            ReasonerService reasonerService
    ) {
        this.kit = kit;
        this.projectInfo = projectInfo;
        this.reasonerService = reasonerService;
    }

    public ModelAndView getOWLClassFragment(
            final OWLClassesService service,
            final OWLClass entity,
            boolean inferred,
            final OWLOntology ont,
            List<With> with,
            final Model model,
            final String queryString
    ) {
        model.addAttribute("type", "Classes");

        var title = getTitle(entity, ont, service);

        return buildCommonEntityFragment(service, entity, inferred, ont, with, model, queryString, title);
    }

    // TODO custom renderer - linked to tree (see relations)
    public ModelAndView getOWLIndividualFragment(
            final OWLIndividualsService service,
            final OWLNamedIndividual entity,
            final boolean inferred,
            List<With> with,
            final OWLOntology ont,
            final Model model,
            final String queryString
    ) {
        model.addAttribute("type", "Individuals");

        var title = getTitle(entity, ont, service);

        return buildCommonEntityFragment(service, entity, inferred, ont, with, model, queryString, title);
    }

    private <T extends OWLEntity> String getTitle(T entity, OWLOntology ont, NamedTypeProvider<T> namedTypesProvider) {

        var sfp = kit.getShortFormProvider();
        var entityName = sfp.getShortForm(entity);

        var namedTypes = namedTypesProvider.getNamedTypes(entity, ont);

        String types = String.join(", ", namedTypes.stream().map(sfp::getShortForm).toList());
        return entityName + (types.isEmpty() ? "" : " (" + types + ")");
    }

    public <T extends OWLEntity> ModelAndView buildCommonEntityFragment(
            CharacteristicsProvider<T> service,
            T entity,
            boolean inferred,
            OWLOntology ont,
            List<With> with,
            Model model,
            String queryString,
            String title
    ) {

        if (projectInfo.activeProfiles().contains("graph")) {
            var mos = new MOSStringRenderer(kit.getFinder(), ont);
            model.addAttribute("graphLink", new GraphURLScheme(mos).getURLForOWLObject(entity, ont));
        }

        var mos = (OWLHTMLRenderer) model.getAttribute("mos");
        if (mos != null) {
            mos.withActiveObject(entity);
        }

        var characteristics = new ArrayList<>(
                service.getCharacteristicsBuilder(entity, ont, kit.getComparator(),
                        with, DEFAULT_PAGE_SIZE).getCharacteristics());

        if (inferred) {
            OWLReasoner reasoner = reasonerService.getReasoner();
            characteristics.addAll(service.getInferredCharacteristics(entity, reasoner));
        }

        model.addAttribute("title", title);
        model.addAttribute("iri", entity.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(queryString, with));

        return new ModelAndView("owlentityfragment");
    }

    public ModelAndView getClassChildren(
            final OWLClass cls,
            final OWLReasoner r,
            final Stats stats,
            final Model model) {

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, treeComparator());
        Tree<OWLClass> prunedTree = hierarchyService.getChildren(cls);

        model.addAttribute("t", prunedTree);
        model.addAttribute("stats", stats);
        model.addAttribute("statsName", stats.getName());

        return new ModelAndView("tree::children");
    }
}
