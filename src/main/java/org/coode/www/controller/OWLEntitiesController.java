package org.coode.www.controller;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.SearchResult;
import org.coode.www.model.SearchResults;
import org.coode.www.service.NameService;
import org.coode.www.service.SearchService;
import org.semanticweb.owlapi.model.OWLEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Controller
@RequestMapping(value="/entities")
@SessionAttributes("kit")
public class OWLEntitiesController extends ApplicationController {

    @Autowired
    private SearchService service;

    @Autowired
    private NameService nameService;

    @RequestMapping(value="/", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody SearchResults find(
            @ModelAttribute("kit") final OWLHTMLKit kit,
            @RequestParam(required=true) final String name) throws OntServerException {

        List<OWLEntity> entities = service.findByName(name, kit);

        SearchResults results = new SearchResults();

        for (OWLEntity owlEntity: entities) {
            results.addResult(new SearchResult(kit.getURLScheme().getURLForOWLObject(owlEntity).toString(), "", nameService.getName(owlEntity, kit)));
        }

        return results;
    }
}
