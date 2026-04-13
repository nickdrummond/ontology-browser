package org.ontbrowser.www.backend.memory.kit;

import org.ontbrowser.www.configuration.Config;
import org.ontbrowser.www.controller.AppStatus;

public interface Restartable {

    void restart(Config config);

    void restart();

    AppStatus getStatus();
}
