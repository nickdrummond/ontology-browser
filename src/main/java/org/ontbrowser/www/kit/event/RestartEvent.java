package org.ontbrowser.www.kit.event;

import org.springframework.context.ApplicationEvent;

public class RestartEvent extends ApplicationEvent {
    public RestartEvent(Object source) {
        super(source);
    }
}

