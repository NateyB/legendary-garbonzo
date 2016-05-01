package com.natebeckemeyer.school.advai.project1.RussellPackage;

import java.util.Random;

import com.natebeckemeyer.school.advai.project1.Main.*;

public class RussellWorld implements World
{

    private final double tolerance = 1E-12;

    private Random rnd = new Random();
    public RussellWorld()
    {

    }

    private double getHeight()
    {
        return 3;
    }

    private double getWidth()
    {
        return 4;
    }

    private double [] makeWithinBounds(double [] state)
    {
        double y = Math.max(Math.min(state[0], getHeight() - tolerance), 0);
        double x = Math.max(Math.min(state[1], getWidth() - tolerance), 0);

        return new double [] {y, x};
    }

    private double [] makeWithinBounds(double [] state, String action)
    {
        double y = state[0];
        double x = state[1];

        if (get(y,x).getType().equalsIgnoreCase("wall"))
        {
            switch (action) {
                case "north": y = 2;
                    break;

                case "west": x = 2;
                    break;

                case "east": x = 1 - tolerance;
                    break;

                case "south": y = 1 - tolerance;
                    break;
            }
        }

        return makeWithinBounds(new double [] {y, x});
    }

    private Panel[][] implementation = {
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("terminal", 1)},
            {new Panel("open"), new Panel("wall"), new Panel("open"), new Panel("terminal", -1)},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
    };

    private Panel get(double y, double x)
    {
        double[] newState = makeWithinBounds(new double[] {Math.floor(y), Math.floor(x)});
        int row = (int) newState[0];
        int col = (int) newState[1];

        return implementation[row][col];
    }

    /**
     * @param state Coordinates for a location in the world
     * @return The actions available in state
     */
    @Override public String[] getActions(double[] state)
    {
        return new String[] {"north", "south", "east", "west"};
    }

    /**
     * @param state  Coordinates for a location in the world
     * @param action An action to apply
     * @return The new state resulting from applying action in state
     */
    @Override public double[] applyAction(double[] state, String action)
    {
        double gaussianMagnitude = rnd.nextGaussian()*.1 + .5;
        double gaussianAngle = rnd.nextGaussian()*(1./4.*Math.PI);

        double [] nextState = new double[2];

        switch (action)
        {
            case "north":
                gaussianAngle = gaussianAngle + Math.PI/2;
                nextState[0] = state[0] - gaussianMagnitude*Math.sin(gaussianAngle);
                nextState[1] = state[1] + gaussianMagnitude*Math.cos(gaussianAngle);
                break;
            case "south":
                gaussianAngle = gaussianAngle - Math.PI/2;
                nextState[0] = state[0] - gaussianMagnitude*Math.sin(gaussianAngle);
                nextState[1] = state[1] + gaussianMagnitude*Math.cos(gaussianAngle);
                break;
            case "west":
                gaussianAngle = gaussianAngle + Math.PI;
                nextState[0] = state[0] - gaussianMagnitude*Math.sin(gaussianAngle);
                nextState[1] = state[1] + gaussianMagnitude*Math.cos(gaussianAngle);
                break;
            case "east":
                nextState[0] = state[0] - gaussianMagnitude*Math.sin(gaussianAngle);
                nextState[1] = state[1] + gaussianMagnitude*Math.cos(gaussianAngle);
                break;
            default:
                System.err.printf("Invalid action %s.%n", action);
                return new double[]{-1000, -1000};
        }
        return makeWithinBounds(nextState, action);
    }

    /**
     * Returns whether the state is terminal or not
     *
     * @param state Coordinates for a location in the world
     */
    @Override public boolean isTerminal(double[] state)
    {
        return get(state[0], state[1]).getType().equalsIgnoreCase("terminal");
    }

    /**
     * @param state Coordinates for a location in the world
     * @return Reward for being in that state
     */
    @Override public double getReward(double[] state)
    {
        return get(state[0], state[1]).getReward();
    }

    /**
     * @return A good starting state for exploring
     */
    @Override public double[] getInitialState()
    {
        return new double[]{rnd.nextDouble()*getHeight(), rnd.nextDouble()*getWidth()};
    }

    /**
     * @param learner The agent whose policy will be examined
     * @return A human-readable policy
     */
    @Override public String getPolicy(Agent learner)
    {
        String policy = "";

        for (int r = 0; r < getHeight()*20; r++)
        {
            for (int c = 0; c < getWidth()*20; c++)
            {
                switch (learner.getBestAction(new double[]{r/20., c/20.}))
                {
                    case "south":
                        policy += "v";
                        break;

                    case "north":
                        policy += "^";
                        break;

                    case "west":
                        policy += "<";
                        break;

                    case "east":
                        policy += ">";
                        break;
                }
                policy += "\t";
            }
            policy += "\n";
        }

        return policy;
    }

    public void displayWorld()
    {
        for (Panel[] row : implementation)
        {
            for (Panel col : row)
            {
                switch (col.getType())
                {
                    case "open":
                        System.out.print("o");
                        break;

                    case "wall":
                        System.out.print("w");
                        break;

                    case "terminal":
                        System.out.print("t");
                        break;

                    default:
                        System.out.print("?");
                        break;
                }
            }
            System.out.println();
        }
    }
}
