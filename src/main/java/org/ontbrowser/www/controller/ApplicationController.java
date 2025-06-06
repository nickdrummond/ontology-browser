package org.ontbrowser.www.controller;

import org.ontbrowser.www.feature.stats.EntityCounts;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.RendererFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ModelAttribute;

public abstract class ApplicationController {

    public static final int DEFAULT_PAGE_SIZE = 30;
    public static final String DEFAULT_PAGE_SIZE_STR = "30";

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Autowired
    protected ProjectInfo projectInfo;

    @Autowired
    protected OWLHTMLKit kit;

    @Autowired
    protected RendererFactory rendererFactory;

    @Autowired
    private StatsService statsService;

    @ModelAttribute("projectInfo")
    public ProjectInfo getProjectInfo() {
        return projectInfo;
    }

    @ModelAttribute("entityCounts")
    public EntityCounts getEntityCounts() {
        return statsService.getEntityCountsTotal();
    }
}
