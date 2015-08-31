package org.coode.www.controller;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.OptionSet;
import org.coode.www.service.OWLOntologiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.SessionAttributes;

import javax.servlet.http.HttpServletResponse;

@Controller
@RequestMapping(value="/options")
@SessionAttributes("kit")
public class OptionsController extends ApplicationController {

    @Autowired
    private ReasonerFactoryService reasonerService;

    @Autowired
    private OWLOntologiesService OWLOntologiesService;

    @RequestMapping
    public String getOptions(@ModelAttribute("kit") final OWLHTMLKit kit,
                             final Model model) throws OntServerException {

        model.addAttribute("reasoners", reasonerService.getAvailableReasoners());

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
}
