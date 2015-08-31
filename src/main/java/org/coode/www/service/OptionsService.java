package org.coode.www.service;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.OptionSet;
import org.coode.www.model.ServerConfig;

public interface OptionsService {
    boolean setOption(OptionSet optionSet, OWLHTMLKit kit) throws OntServerException;

    ServerConfig getConfig(OWLHTMLKit kit);
}
