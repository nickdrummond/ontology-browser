package org.coode.www.service;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.OptionSet;
import org.coode.www.model.ServerConfig;
import org.coode.www.repository.KitRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OptionsService {

    @Autowired
    private KitRepository kitRepository;

    public boolean setOption(final OptionSet optionSet, final OWLHTMLKit kit) throws OntServerException {
        ServerConfig config = kit.getConfig();

        ServerConfig newConfig = config.setOption(optionSet);

        if (newConfig != config) {
            kit.setConfig(newConfig);
            kitRepository.saveKit(kit);
            return true;
        }
        return false;
    }

    public ServerConfig getConfig(final OWLHTMLKit kit) {
        return kit.getConfig();
    }
}
