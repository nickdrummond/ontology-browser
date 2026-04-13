package org.ontbrowser.www.controller;

import org.ontbrowser.www.backend.BackendContext;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/status")
public class StatusController {

    private final BackendContext backend;

    public StatusController(BackendContext backend) {
        this.backend = backend;
    }

    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public AppStatus status() {
        return backend.getStatus();
    }
}
