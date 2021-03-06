package com.natebeckemeyer.school.advai.project1.exercise21A;

import com.natebeckemeyer.school.advai.project1.main.Agent;
import com.natebeckemeyer.school.advai.project1.main.Panel;
import com.natebeckemeyer.school.advai.project1.main.World;

import java.util.ArrayList;
import java.util.Random;

public class Exercise21AWorld implements World
{
    private Random rnd = new Random();

    public Exercise21AWorld()
    {

    }

    private double getHeight()
    {
        return 10;
    }

    private double getWidth()
    {
        return 10;
    }

    private Panel[][] implementation = {
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("terminal", 10000)},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")},
            {new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open"), new Panel("open")}
    };


    private double [] makeWithinBounds(double [] state)
    {
        double y = Math.max(Math.min(state[0], getHeight() - 10E-12), 0);
        double x = Math.max(Math.min(state[1], getWidth() - 10E-12), 0);

        return new double [] {y, x};
    }

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
        ArrayList<String> actions = new ArrayList<>();
        double y = state[0];
        double x = state[1];

        if (y >= 1)
        {
            actions.add("north");
        }
        if (y < getHeight() - 1)
        {
            actions.add("south");
        }
        if (x >= 1)
        {
            actions.add("west");
        }
        if (x < getWidth() - 1)
        {
            actions.add("east");
        }

        String[] acts = new String[actions.size()];
        return actions.toArray(acts);
    }

    /**
     * @param state  Coordinates for a location in the world
     * @param action An action to apply
     * @return The new state resulting from applying action in state
     */
    @Override public double[] applyAction(double[] state, String action)
    {
        double gaussianMagnitude;
        double gaussianAngle = rnd.nextGaussian()*(1./4.*Math.PI);
        do{
            gaussianMagnitude = rnd.nextGaussian()*.1;
        } while (Math.abs(gaussianMagnitude) > .5);

        gaussianMagnitude += .5;

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
                System.err.println("Invalid action.");
                return new double[]{-1000, -1000};
        }
        return makeWithinBounds(nextState);
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

        for (int i = 0; i < getHeight()*10; i++)
        {
            for (int j = 0; j < getWidth()*10; j++)
            {
                double r = i/10.;
                double c = j/10.;
                switch (learner.getBestAction(new double[]{r, c}))
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

                    default:
                        policy += "?";
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
