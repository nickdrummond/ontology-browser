package org.ontbrowser.www.backend.db;

import org.ontbrowser.www.feature.stats.EntityExists;
import org.ontbrowser.www.feature.stats.StatsService;
import org.semanticweb.owlapi.model.EntityType;
import org.semanticweb.owlapi.model.parameters.Imports;
import owlapi.DBOntology;
import owlapi.DBStats;

import java.sql.Connection;
import java.sql.SQLException;

public class DBStatsService extends StatsService {
    private final DBContext dbContext;
    private final Connection connection;

    public DBStatsService(DBContext dbContext, Connection connection) {
        super(dbContext);
        this.dbContext = dbContext;
        this.connection = connection;
    }

    @Override
    public EntityExists entityExists() {
        var dbStats = new DBStats(connection);
        var rootOntology = (DBOntology) dbContext.getRootOntology();
        var imports = Imports.INCLUDED; // TODO parameter?
        try {
            return new EntityExists(
                    dbStats.containsEntitiesOfType(rootOntology, EntityType.CLASS, imports),
                    dbStats.containsEntitiesOfType(rootOntology, EntityType.NAMED_INDIVIDUAL, imports),
                    dbStats.containsEntitiesOfType(rootOntology, EntityType.OBJECT_PROPERTY, imports),
                    dbStats.containsEntitiesOfType(rootOntology, EntityType.DATA_PROPERTY, imports),
                    dbStats.containsEntitiesOfType(rootOntology, EntityType.ANNOTATION_PROPERTY, imports),
                    dbStats.containsEntitiesOfType(rootOntology, EntityType.DATATYPE, imports)
            );
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    // TODO override stats that can be more efficiently retrieved from the DB - eg counts
}
