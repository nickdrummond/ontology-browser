package org.coode.www.controller;

import org.coode.www.url.CommonRelationsURLScheme;
import org.coode.www.url.URLScheme;
import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.renderer.RendererFactory;
import org.coode.www.service.*;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;

public class CommonRelations<T extends OWLProperty> {

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
            Model model,
            Comparator<OWLObject> comparator) throws NotFoundException {
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ont);
        renderEntity(individual, model);
        model.addAttribute("characteristics", individualsService.getCharacteristics(individual, ont, comparator));
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
}
