package RussellPackage;

import Main.Features;
import Main.Vector;

import java.util.Random;

public class RussellRadial implements Features
{
    private Random rnd = new Random();
    private RussellWorld world = new RussellWorld();

    /**
     * Initial parameter values for theta (function approximation)
     *
     * @return The initial vector
     */
    @Override public Vector getInitialParameters()
    {
        Vector params = new Vector();
        for (int i = 0; i < 4; i++)
        {
            for (int j = 0; j < 5; j++)
            {
                for (String act : world.getActions(new double[]{i, j}))
                {
                    params.add(0);
                }
            }
        }

        return params;
    }

    /**
     * @param state  Coordinates in the world
     * @param action The action applied
     * @return The vector of activations
     */
    @Override public Vector getActivations(double[] state, String action)
    {
        double y = state[0];
        double x = state[1];
        Vector activations = new Vector();
        for (int i = 0; i < 4; i++)
        {
            for (int j = 0; j < 5; j++)
            {
                for (String act : world.getActions(new double[]{i, j}))
                {
                    if (action.equalsIgnoreCase(act))
                        activations.add(Math.exp(-(Math.pow(y - i, 2) + Math.pow(x - j, 2))/2));
                    else
                        activations.add(0);
                }
            }
        }
        return activations;
    }
}
