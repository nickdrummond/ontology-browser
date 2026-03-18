package org.ontbrowser.www.configuration;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletRequest;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;

public class DBConnectionFilter implements Filter {
    private final DataSource dataSource;

    public static final String DB_CONNECTION_ATTR = "dbConnection";

    public DBConnectionFilter(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        if (request instanceof HttpServletRequest) {
            Connection conn = null;
            try {
                conn = dataSource.getConnection();
                request.setAttribute(DB_CONNECTION_ATTR, conn);
                chain.doFilter(request, response);
            } catch (SQLException e) {
                throw new RuntimeException(e);
            } finally {
                if (conn != null) {
                    try { conn.close(); } catch (Exception ignored) {}
                }
                request.removeAttribute(DB_CONNECTION_ATTR);
            }
        } else {
            chain.doFilter(request, response);
        }
    }
}

