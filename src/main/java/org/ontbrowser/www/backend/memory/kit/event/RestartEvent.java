package org.ontbrowser.www.backend.memory.kit.event;

import org.springframework.context.ApplicationEvent;

public class RestartEvent extends ApplicationEvent {
    public RestartEvent(Object source) {
        super(source);
    }
}

