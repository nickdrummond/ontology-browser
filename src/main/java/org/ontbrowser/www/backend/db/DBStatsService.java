package org.ontbrowser.www.backend.db;

import org.ontbrowser.www.feature.stats.EntityCounts;
import org.ontbrowser.www.feature.stats.EntityExists;
import org.ontbrowser.www.feature.stats.StatsService;
import org.semanticweb.owlapi.model.EntityType;
import org.semanticweb.owlapi.model.OWLOntology;
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

    @Override
    public EntityCounts getCounts(OWLOntology ont, Imports imports) {
        var dbStats = new DBStats(connection);
        if (ont instanceof DBOntology dbOnt) {
            try {
                return new EntityCounts(
                        dbStats.countEntitiesOfType(dbOnt, EntityType.CLASS, imports),
                        dbStats.countEntitiesOfType(dbOnt, EntityType.NAMED_INDIVIDUAL, imports),
                        dbStats.countEntitiesOfType(dbOnt, EntityType.OBJECT_PROPERTY, imports),
                        dbStats.countEntitiesOfType(dbOnt, EntityType.DATA_PROPERTY, imports),
                        dbStats.countEntitiesOfType(dbOnt, EntityType.ANNOTATION_PROPERTY, imports),
                        dbStats.countEntitiesOfType(dbOnt, EntityType.DATATYPE, imports)
                );
            } catch (SQLException e) {
                    throw new RuntimeException(e);
            }
        } else {
            return super.getCounts(ont, imports);
        }
    }
}
