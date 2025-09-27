package org.ontbrowser.www.kit;

import org.ontbrowser.www.controller.AppStatus;

// TODO finding the listeners should be done by Spring config
public interface Restartable {

    void restart(Config config);

    void restart();

    AppStatus getStatus();

    void registerListener(RestartListener listener);
}
