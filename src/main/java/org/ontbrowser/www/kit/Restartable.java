package org.ontbrowser.www.kit;

import org.ontbrowser.www.controller.AppStatus;

public interface Restartable {

    void restart(Config config);

    void restart();

    AppStatus getStatus();
}
