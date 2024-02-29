package org.coode.www.controller;

import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.With;
import org.coode.www.url.CommonRelationsURLScheme;
import org.coode.www.url.ComponentPagingURIScheme;
import org.coode.www.url.URLScheme;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.renderer.RendererFactory;
import org.coode.www.service.*;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.Relation;
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

        Comparator<Tree<Relation<T>>> comparator = propertiesService.getComparator(orderByProperty, ont);

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

        Tree<Relation<T>> relationsTree = (individual != null) ?
                relationsHierarchyService.getPrunedTree(new Relation<>(relationsHierarchyService.getProperty(), individual)) :
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
