/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.ServerProperties;
import org.coode.owl.mngr.ServerPropertiesAdapter;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 22, 2008<br><br>
 */
public class ServerPropertiesAdapterImpl<E extends Enum> implements ServerPropertiesAdapter<E> {

    protected ServerProperties delegate;


    public ServerPropertiesAdapterImpl(ServerProperties delegate) {
        this.delegate = delegate;
    }

    // share the delegate
    public ServerPropertiesAdapterImpl(ServerPropertiesAdapterImpl anotherAdapter) {
        this.delegate = anotherAdapter.delegate;
    }


    public void addPropertyChangeListener(PropertyChangeListener l) {
        delegate.addPropertyChangeListener(l);
    }


    public void removePropertyChangeListener(PropertyChangeListener l) {
        delegate.removePropertyChangeListener(l);
    }


    public void save(OutputStream out) throws IOException {
        delegate.save(out);
    }


    public void load(InputStream in) throws IOException {
        delegate.load(in);
    }


    public void addDeprecatedNames(Map<String, String> old2NewNames) {
        delegate.addDeprecatedNames(old2NewNames);
    }


    public void setBoolean(E key, boolean b) {
        delegate.set(key.name(), Boolean.toString(b));
    }


    public String get(E key) {
        return delegate.get(key.name());
    }


    public boolean set(E key, String value) {
        return delegate.set(key.name(), value);
    }


    public boolean isSet(E key) {
        return delegate.isSet(key.name());
    }

    public URL getURL(E key) throws MalformedURLException {
        return delegate.getURL(key.name());
    }


    public void remove(E key) {
        delegate.remove(key.name());
    }


    public void setAllowedValues(E key, List<String> values) {
        delegate.setAllowedValues(key.name(), values);
    }


    public List<String> getAllowedValues(E key) {
        return delegate.getAllowedValues(key.name());
    }
}