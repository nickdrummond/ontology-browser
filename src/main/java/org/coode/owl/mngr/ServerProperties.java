package org.coode.owl.mngr;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Deprecated
public interface ServerProperties {

    String get(String key);

    Map<String, String> getAll();

    URL getURL(String key) throws MalformedURLException;

    boolean set(String key, String value);

    Set<String> keySet();

    void remove(String key);    

    void addPropertyChangeListener(PropertyChangeListener l);

    void removePropertyChangeListener(PropertyChangeListener l);

    void save(OutputStream out) throws IOException;

    void load(InputStream in) throws IOException;

    void addDeprecatedNames(Map<String, String> names);

    boolean isSet(String booleanOption);

    void setAllowedValues(String key, List<String> values);

    List<String> getAllowedValues(String key);
}
