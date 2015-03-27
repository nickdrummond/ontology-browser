package org.coode.www.mngr;

public class Application {

    private static SessionManager sessionManager;
    private static KitRepository repo;

    public static SessionManager getSessionManager() {
        if (sessionManager == null) {
            sessionManager = new SessionManager(getRepo());
        }
        return sessionManager;
    }

    public static KitRepository getRepo() {
        if (repo == null) {
            repo = new KitRepository();
        }
        return repo;
    }
}
