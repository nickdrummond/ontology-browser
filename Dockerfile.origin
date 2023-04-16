FROM alpine/git as clone
WORKDIR /app
RUN git clone https://github.com/nickdrummond/ontology-browser.git

FROM maven:3.9.0-eclipse-temurin-11-alpine as build
WORKDIR /app
COPY --from=clone /app/ontology-browser /app
RUN mvn package

FROM jetty:10.0.14-jre11-alpine
COPY --from=build /app/target/ontology-browser.war /var/lib/jetty/webapps/ROOT.war
EXPOSE 8080

