package RussellPackage;

import Main.Features;
import Main.Vector;

import java.util.Random;

public class RussellTiling implements Features
{
    private Random rnd = new Random();
    private double offsetX = rnd.nextDouble();
    private double offsetY = rnd.nextDouble();
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
        Vector activations = new Vector();
        int r1 = (int) Math.floor(state[0]);
        int c1 = (int) Math.floor(state[1]);

        int r2 = (int) Math.floor(state[0] - offsetY);
        int c2 = (int) Math.floor(state[1] - offsetX);

        for (int i = 0; i < 4; i++)
        {
            for (int j = 0; j < 5; j++)
            {
                for (String act : world.getActions(new double[]{i, j}))
                {
                    if (r1 == i && c1 == j && act.equalsIgnoreCase(action))
                    {
                        if (r2 == i && c2 == j)
                        {
                            activations.add(2);
                        } else
                        {
                            activations.add(1);
                        }
                    } else
                    {
                        if (r2 == i && c2 == j)
                        {
                            activations.add(1);
                        } else
                        {
                            activations.add(0);
                        }
                    }
                }
            }
        }

        return activations;
    }
}
