package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.RestartListener;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.renderer.RendererFactory;
import org.ontbrowser.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.service.hierarchy.OWLHierarchyService;
import org.ontbrowser.www.service.stats.StatsMemo;
import org.ontbrowser.www.service.stats.StatsService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ui.Model;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

public class CommonRelations<T extends OWLProperty> implements RestartListener {

    private static final Logger log = LoggerFactory.getLogger(CommonRelations.class);

    public static final String BASE_TREE = "base::children";

    private final String path;
    private final OWLHTMLKit kit;
    private final PropertiesService<T> propertiesService;
    private final OWLIndividualsService individualsService;
    private final RendererFactory rendererFactory;
    private final StatsService statsService;

    private final Map<StatsMemo, AbstractRelationsHierarchyService<T>> hierarchyCache = new HashMap<>();

    public CommonRelations(
            String path,
            OWLHTMLKit kit,
            PropertiesService<T> propertiesService,
            OWLIndividualsService individualsService,
            @Nonnull RendererFactory rendererFactory,
            StatsService statsService) {
        this.path = path;
        this.kit = kit;
        kit.registerListener(this);
        this.propertiesService = propertiesService;
        this.individualsService = individualsService;
        this.rendererFactory = rendererFactory;
        this.statsService = statsService;
    }

    public void renderEntity(OWLEntity entity, Model model) {
        String shortForm = kit.getShortFormProvider().getShortForm(entity);
        String type = entity.getEntityType().getPrintName();
        model.addAttribute("title", shortForm + " (" + type + ")");
        model.addAttribute("iri", entity.getIRI());
    }

    public OWLNamedIndividual renderIndividual(
            String individualId,
            OWLOntology ont,
            List<With> with,
            int pageSize,
            HttpServletRequest request,
            Model model,
            Comparator<OWLObject> comparator) {

        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ont);

        renderEntity(individual, model);

        List<Characteristic> characteristics = individualsService.getCharacteristics(individual, ont, comparator, with, pageSize);

        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, with));

        return individual;
    }

    public AbstractRelationsHierarchyService<T> getRelationsHierarchyService(
            T property,
            OWLOntology ont,
            @Nullable String orderBy,
            boolean inverse) {

        T orderByProperty = (orderBy != null) ? propertiesService.getPropertyFor(orderBy, ont) : null;

        Comparator<Tree<OWLNamedIndividual>> comparator = propertiesService.getComparator(orderByProperty, ont);

        StatsMemo memo = new StatsMemo(property.getIRI().toString(), String.valueOf(inverse), ont.getOntologyID().toString());

        if (hierarchyCache.containsKey(memo)) {
            return hierarchyCache.get(memo);
        }

        var hierarchy = propertiesService
                .getRelationsHierarchy(comparator)
                .withProperties(property, ont, inverse);
        hierarchyCache.put(memo, hierarchy);
        return hierarchy;
    }

   public void buildPrimaryTree(
           T property,
           OWLHierarchyService<? super T> hierarchyService,
           String title,
           Model model
   ) {
       model.addAttribute("type", title);
       model.addAttribute("hierarchy", hierarchyService.getPrunedTree(property));
   }

    public void buildSecondaryTree(
            AbstractRelationsHierarchyService<T> relationsHierarchyService,
            @Nullable OWLNamedIndividual individual,
            Model model,
            HttpServletRequest request) {

        T property =  relationsHierarchyService.getProperty();

        Tree<OWLNamedIndividual> relationsTree = (individual != null) ?
                relationsHierarchyService.getPrunedTree(individual) :
                relationsHierarchyService.getClosedTree();

        model.addAttribute("type2", kit.getShortFormProvider().getShortForm(property));
        model.addAttribute("inverse", relationsHierarchyService.isInverse());
        model.addAttribute("mos", getRenderer(relationsHierarchyService, property, individual, request));
        model.addAttribute("hierarchy2", relationsTree);
        //TODO fix this - generics!!
        model.addAttribute("stats2", statsService.getTreeStats(createMemo(relationsHierarchyService), relationsHierarchyService));
        model.addAttribute("statsName2", "treeStats");
    }

    public OWLHTMLRenderer getRenderer(
            AbstractRelationsHierarchyService<T> relationsHierarchyService,
            T property,
            @Nullable OWLNamedIndividual individual,
            HttpServletRequest request) {

        URLScheme urlScheme = getUrlScheme(relationsHierarchyService, property, request);

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(property);
        if (individual != null) {
            activeObjects.add(individual);
        }

        return rendererFactory.getHTMLRenderer(relationsHierarchyService.getOnt())
                .withActiveObjects(activeObjects)
                .withURLScheme(urlScheme);
    }

    private URLScheme getUrlScheme(AbstractRelationsHierarchyService<T> relationsHierarchyService, T property, HttpServletRequest request) {
        return new CommonRelationsURLScheme<>("/relations/" + path, property)
                .withTree(relationsHierarchyService)
                .withQuery(request.getQueryString());
    }

    public OWLNamedIndividual getOWLIndividualFor(String individualId, OWLOntology ont) {
        return individualsService.getOWLIndividualFor(individualId, ont);
    }

    public StatsMemo createMemo(AbstractRelationsHierarchyService<?> hierarchyService) {
        return new StatsMemo(
                hierarchyService.getProperty().toString(),
                hierarchyService.getOnt().getOntologyID().toString(),
                Boolean.toString(hierarchyService.isInverse()));
    }

    @Override
    public void onRestart() {
        log.info("Clearing hierarchy cache");
        hierarchyCache.clear();
    }
}
