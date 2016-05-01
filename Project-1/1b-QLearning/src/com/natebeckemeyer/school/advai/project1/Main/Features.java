package com.natebeckemeyer.school.advai.project1.Main;

public interface Features
{
    /**
     * Initial parameter values for theta (function approximation)
     * @return The initial vector
     */
    Vector getInitialParameters();

    /**
     * @param state Coordinates in the world
     * @param action The action applied
     * @return The vector of activations
     */
    Vector getActivations(double [] state, String action);
}
