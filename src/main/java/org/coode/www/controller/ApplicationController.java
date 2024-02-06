package org.coode.www.controller;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ProjectInfo;
import org.coode.www.renderer.RendererFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ModelAttribute;

public abstract class ApplicationController {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Autowired
    protected ProjectInfo projectInfo;

    @Autowired
    protected OWLHTMLKit kit;

    @Autowired
    protected RendererFactory rendererFactory;

    @ModelAttribute("projectInfo")
    public ProjectInfo getProjectInfo() {
        return projectInfo;
    }

    @ModelAttribute("kit")
    public OWLHTMLKit getKit() { return kit; }

}
