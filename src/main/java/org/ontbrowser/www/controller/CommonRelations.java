package org.ontbrowser.www.controller;

import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.service.OWLIndividualsService;
import org.ontbrowser.www.service.PropertiesService;
import org.ontbrowser.www.url.CommonRelationsURLScheme;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.ontbrowser.www.url.URLScheme;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.renderer.RendererFactory;
import org.ontbrowser.www.service.*;
import org.ontbrowser.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.ui.Model;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.*;

public class CommonRelations<T extends OWLProperty> {

    public static final String BASE_TREE = "base :: tree";

    private final String path;
    private final ShortFormProvider sfp;
    private final PropertiesService<T> propertiesService;
    private final OWLIndividualsService individualsService;
    private final RendererFactory rendererFactory;

    public CommonRelations(
            String path,
            ShortFormProvider sfp,
            PropertiesService<T> propertiesService,
            OWLIndividualsService individualsService,
            @Nonnull RendererFactory rendererFactory) {
        this.path = path;
        this.sfp = sfp;
        this.propertiesService = propertiesService;
        this.individualsService = individualsService;
        this.rendererFactory = rendererFactory;
    }

    public void renderEntity(OWLEntity entity, Model model) {
        String shortForm = sfp.getShortForm(entity);
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
            Comparator<OWLObject> comparator) throws NotFoundException {

        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ont);

        renderEntity(individual, model);

        List<Characteristic> characteristics = individualsService.getCharacteristics(individual, ont, comparator, with, pageSize);

        model.addAttribute("characteristics", characteristics);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, with));

        return individual;
    }

    public AbstractRelationsHierarchyService<T> getRelationsHierarchyService(
            T property,
            OWLOntology ont,
            @Nullable String orderBy,
            boolean inverse) throws NotFoundException {

        T orderByProperty = (orderBy != null) ? propertiesService.getPropertyFor(orderBy, ont) : null;

        Comparator<Tree<OWLNamedIndividual>> comparator = propertiesService.getComparator(orderByProperty, ont);

        return propertiesService
                .getRelationsHierarchy(comparator)
                .withProperties(property, ont, inverse);
    }

    public void buildCommon(
            AbstractRelationsHierarchyService<T> relationsHierarchyService,
            @Nullable OWLNamedIndividual individual,
            OWLOntology ont,
            Model model,
            HttpServletRequest request) {

        T property =  relationsHierarchyService.getProperty();

        URLScheme urlScheme = new CommonRelationsURLScheme<>(relationsHierarchyService,
                "/relations/" + path, property).withQuery(request.getQueryString());

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(property);
        if (individual != null) {
            activeObjects.add(individual);
        }

        Tree<OWLNamedIndividual> relationsTree = (individual != null) ?
                relationsHierarchyService.getPrunedTree(individual) :
                relationsHierarchyService.getTree();

        OWLHTMLRenderer renderer = rendererFactory.getRenderer(ont).withActiveObjects(activeObjects).withURLScheme(urlScheme);

        model.addAttribute("type", "Relations on");
        model.addAttribute("hierarchy", propertiesService.getPropTree(property, ont));
        model.addAttribute("type2", sfp.getShortForm(property));
        model.addAttribute("inverse", relationsHierarchyService.isInverse());
        model.addAttribute("mos", renderer);
        model.addAttribute("hierarchy2", relationsTree);
    }

    public OWLNamedIndividual getOWLIndividualFor(String individualId, OWLOntology ont) throws NotFoundException {
        return individualsService.getOWLIndividualFor(individualId, ont);
    }
}
