package com.natebeckemeyer.school.advai.project1.Main;

public interface World
{

    /**
     * @param state Coordinates for a location in the world
     * @return The actions available in state
     */
    String[] getActions(double [] state);

    /**
     * @param state Coordinates for a location in the world
     * @param action An action to apply
     * @return The new state resulting from applying action in state
     */
    double [] applyAction(double [] state, String action);

    /**
     * Returns whether the state is terminal or not
     */
    boolean isTerminal(double [] state);

    /**
     * @param state Coordinates for a location in the world
     * @return Reward for being in that state
     */
    double getReward(double [] state);

    /**
     * @return A good starting state for exploring
     */
    double [] getInitialState();

    /**
     * @param learner The agent whose policy will be examined
     * @return A human-readable policy
     */
    String getPolicy(Agent learner);
}
