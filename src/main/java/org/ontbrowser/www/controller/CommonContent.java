package org.ontbrowser.www.controller;

import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.ontbrowser.www.url.PagingURIScheme;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.stereotype.Service;
import org.springframework.ui.Model;

import java.net.URI;
import java.util.Comparator;

/**
 * This class should contain any model data required for headers and footers
 */
@Service
public class CommonContent {

    protected final ProjectInfo projectInfo;

    protected final BackendContext backend;

    private final StatsService statsService;

    public CommonContent(
            ProjectInfo projectInfo,
            BackendContext backend,
            StatsService statsService
    ) {
        this.projectInfo = projectInfo;
        this.backend = backend;
        this.statsService = statsService;
    }

    public void addCommonContent(String queryString, Model model, OWLOntology ont) {
        // required for header and footer text and links
        model.addAttribute("projectInfo", projectInfo);
        // required for entity visibility in the menu
        model.addAttribute("entityCounts", statsService.getEntityCountsTotal());

        var scheme = new GlobalPagingURIScheme(queryString);
        model.addAttribute("pageURIScheme", scheme);

        addOntologySelectorData(model, ont, scheme);
    }

    record OntologySummary(String label, URI link) {}

    private void addOntologySelectorData(Model model, OWLOntology ont, PagingURIScheme scheme) {
        var ontSfp = backend.getOntologySFP();
        var allOntologies = Imports.INCLUDED.stream(backend.getRootOntology())
                .map(ontology -> new OntologySummary(
                        ontSfp.getShortForm(ontology),
                        scheme.replacingParam("ontId", Integer.toString(ontology.getOntologyID().hashCode()))
                ))
                .sorted(Comparator.comparing(OntologySummary::label))
                .toList();
        model.addAttribute("allOntologies", allOntologies);
        model.addAttribute("ont", ontSfp.getShortForm(ont));
    }
}
