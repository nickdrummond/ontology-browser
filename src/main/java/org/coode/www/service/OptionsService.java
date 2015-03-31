package org.coode.www.service;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.coode.owl.mngr.ServerOptionsAdapter;
import org.coode.owl.mngr.ServerProperty;
import org.coode.www.exception.OntServerException;
import org.coode.www.mngr.Application;
import org.coode.www.model.OptionSet;

import org.springframework.stereotype.Service;

import java.util.Map;

@Service
public class OptionsService {

    public boolean setOption(final OptionSet optionSet, final OWLHTMLKit kit) throws OntServerException {
        // TODO get rid of this split of properties
        boolean success;
        try{
            OWLHTMLProperty property = OWLHTMLProperty.valueOf(optionSet.getProperty());
            ServerOptionsAdapter<OWLHTMLProperty> serverProperties = kit.getHTMLProperties();
            success = serverProperties.set(property, optionSet.getValue());
        }
        catch(IllegalArgumentException e){
            // then this must be an OWL server preference
            try{
                ServerProperty property = ServerProperty.valueOf(optionSet.getProperty());
                success = kit.getOWLServer().getProperties().set(property, optionSet.getValue());
            }
            catch(IllegalArgumentException e2){
                throw new OntServerException("Cannot set unknown property: " + optionSet.getProperty());
            }
        }

        if (success){
            Application.getRepo().saveKit(kit);
        }

        return success;
    }

    public Map<String, String> getOptionsAsMap(final OWLHTMLKit kit) {
        return kit.getHTMLProperties().getAll();
    }

    public OptionSet getOption(final String property, final OWLHTMLKit kit) {
        return new OptionSet(property, getOptionsAsMap(kit).get(property));
    }
}
