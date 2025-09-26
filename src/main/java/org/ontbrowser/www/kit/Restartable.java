package org.ontbrowser.www.kit;

// TODO finding the listeners should be done by Spring config
public interface Restartable {
    void registerListener(RestartListener listener);
}
