package org.coode.www.controller;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.OptionSet;
import org.coode.www.service.OntologiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;

@Controller
@RequestMapping(value="/options")
@SessionAttributes("kit")
public class OptionsController extends ApplicationController {

    @Autowired
    private ReasonerFactoryService reasonerService;

    @Autowired
    private OntologiesService ontologiesService;

    @RequestMapping
    public String getOptions(@ModelAttribute("kit") final OWLHTMLKit kit,
                             final Model model) throws OntServerException {

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("reasoners", reasonerService.getAvailableReasoners());
        model.addAttribute("activeOntology", ontologiesService.getActiveOntology(kit));
        model.addAttribute("ontologies", ontologiesService.getOntologies(kit));

        return "options";
    }

    @RequestMapping(method = RequestMethod.POST)
    public String setOption(
            @ModelAttribute("kit") final OWLHTMLKit kit,
            @ModelAttribute final OptionSet optionSet,
            HttpServletResponse response) throws OntServerException {

        if (optionsService.setOption(optionSet, kit)) {
            return "redirect:/options";
        }
        else {
            response.setStatus(HttpStatus.BAD_REQUEST.value());
            return "Unknown option: " + optionSet.getProperty();
        }
    }

    @RequestMapping(method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    public @ResponseBody OptionSet setOptionJson(
            @ModelAttribute("kit") final OWLHTMLKit kit,
            @ModelAttribute() final OptionSet optionSet) throws OntServerException {

        return optionsService.getOption(optionSet.getProperty(), kit);
    }
}
