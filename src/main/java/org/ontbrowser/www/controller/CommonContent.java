package org.ontbrowser.www.controller;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.RendererFactory;
import org.springframework.stereotype.Service;
import org.springframework.ui.Model;

/**
 * This class should contain any model data required for headers and footers
 */
@Service
public class CommonContent {

    protected final ProjectInfo projectInfo;

    protected final OWLHTMLKit kit;

    protected final RendererFactory rendererFactory;

    private final StatsService statsService;

    public CommonContent(ProjectInfo projectInfo, OWLHTMLKit kit, RendererFactory rendererFactory, StatsService statsService) {
        this.projectInfo = projectInfo;
        this.kit = kit;
        this.rendererFactory = rendererFactory;
        this.statsService = statsService;
    }

    public void addCommonContent(HttpServletRequest request, Model model) {
        // required for header and footer text and links
        model.addAttribute("projectInfo", projectInfo);
        // required for entity visibility in the menu
        model.addAttribute("entityCounts", statsService.getEntityCountsTotal());

        // required for ontology selector
        model.addAttribute("allOntologies", kit.getOntologies());
//        model.addAttribute("ontIdPlaceholderURI", getOntIdPlaceholderURI(request));
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
    }

//    private String getOntIdPlaceholderURI(HttpServletRequest request) {
//        var query = request.getQueryString();
//        if (query != null) {
//            // remove ontId param from query string
//            return request.getRequestURI() + "?" + query.replaceAll("ontId=[^&]*&?", "ontId=[ontId]");
//        }
//        return request.getRequestURI() + "?ontId=[ontId]"; // placeholder for the ontology ID
//    }
}
