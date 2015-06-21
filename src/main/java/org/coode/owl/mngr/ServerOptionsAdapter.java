package org.coode.owl.mngr;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;

@Deprecated
public interface ServerOptionsAdapter<E extends Enum> {

    String get(E key);

    boolean set(E key, String value);

    Map<String, String> getAll();

    boolean isSet(E key);

    URL getURL(E key) throws MalformedURLException;

    void remove(E key);

    void setAllowedValues(E key, List<String> values);

    List<String> getAllowedValues(E key);

    void addPropertyChangeListener(PropertyChangeListener l);

    void removePropertyChangeListener(PropertyChangeListener l);

    void save(OutputStream out) throws IOException;

    void load(InputStream in) throws IOException;

    void addDeprecatedNames(Map<String, String> old2NewNames);

    void setBoolean(E key, boolean b);
}
