package org.coode.www.controller;

import org.coode.www.mngr.SessionManager;
import org.coode.www.model.ApplicationInfo;
import org.springframework.beans.factory.annotation.Autowired;

abstract public class ApplicationController {

    @Autowired
    protected ApplicationInfo applicationInfo;

    @Autowired
    protected SessionManager sessionManager;

}
