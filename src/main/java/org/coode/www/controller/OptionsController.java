package org.coode.www.controller;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.OntServerException;
import org.coode.www.model.OptionSet;
import org.coode.www.service.OntologiesService;
import org.coode.www.service.OptionsService;
import org.coode.www.service.ReasonerFactoryService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

@Controller
@RequestMapping(value="/options")
public class OptionsController extends ApplicationController {

    @Autowired
    private OptionsService optionsService;

    @Autowired
    private ReasonerFactoryService reasonerService;

    @Autowired
    private OntologiesService ontologiesService;

    @RequestMapping
    public String getOptions(final HttpServletRequest request,
                             final Model model) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request);

        model.addAttribute("applicationInfo", applicationInfo);
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("reasoners", reasonerService.getAvailableReasoners());
        model.addAttribute("activeOntology", ontologiesService.getActiveOntology(kit));
        model.addAttribute("ontologies", ontologiesService.getOntologies(kit));
        return "options";
    }

    @RequestMapping(method = RequestMethod.POST)
    public String setOption(
            final HttpServletRequest request,
            @ModelAttribute final OptionSet optionSet) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request);

        optionsService.setOption(optionSet, kit);

        return "redirect:/options";
    }

    @RequestMapping(method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    public @ResponseBody OptionSet setOptionJson(
            final HttpServletRequest request,
            @ModelAttribute() final OptionSet optionSet) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request);

        return optionsService.getOption(optionSet.getProperty(), kit);
    }
}
