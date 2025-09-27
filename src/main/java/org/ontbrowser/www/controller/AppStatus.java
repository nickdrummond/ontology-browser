package org.ontbrowser.www.controller;

public record AppStatus(Status status) {

public enum Status { UP, STARTING, DOWN}
}
