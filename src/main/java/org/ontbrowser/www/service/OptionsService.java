package org.ontbrowser.www.service;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.OptionSet;

public interface OptionsService {
    boolean setOption(OptionSet optionSet, OWLHTMLKit kit);
}
