package org.coode.www.mngr;

public class SessionManagerFactory {

    private static SessionManager instance;

    public static SessionManager getSessionManager() {
        if (instance == null) {
            instance = new SessionManager();
        }
        return instance;
    }
}
