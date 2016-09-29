package com.natebeckemeyer.school.advai.project1.main;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;

public class Agent
{
    /**
     * The eligibility traces for this agent.
     */
    private Vector eligibility;

    /**
     * The utilities (theta parameters) for the linear approximation q-function
     */
    private Vector theta;

    private World world;
    private Features features;

    private Random rnd = new Random();

    //private ArrayList<>

    /**
     * Learning rate
     */
    private double alpha;
    private double lambda;
    private double gamma;

    private String displayIter(int iter, int numTrials, long startTime)
    {
        iter = iter + 1;
        if (iter == 1 || iter % Math.floor(numTrials/10) == 0 || iter == numTrials)
        {
            return String.format("Iteration %d complete. Time taken: %d (ms). %n", iter, 10*(System.currentTimeMillis() - startTime));
        }

        return "";
    }

    private String getMaxAction(HashMap<String, Double> qTable)
    {
        Iterator<Map.Entry<String, Double>> iterator = qTable.entrySet().iterator();

        Map.Entry<String, Double> first = iterator.next();
        String argmax = first.getKey();
        double max = first.getValue();

        while (iterator.hasNext())
        {
            Map.Entry<String, Double> cur = iterator.next();
            if (cur.getValue() > max)
            {
                argmax = cur.getKey();
                max = cur.getValue();
            }
        }

        assert argmax != null;

        return argmax;
    }

    public String getBestAction(double[] state)
    {
        HashMap<String, Double> qTable = new HashMap<>();
        for (String action : world.getActions(state))
        {
            qTable.put(action, features.getActivations(state, action).dot(theta));
        }
        return getMaxAction(qTable);
    }

    private double[] episodeStep(double[] state, double epsilon)
    {
        String action = getBestAction(state);
        if (rnd.nextDouble() < epsilon)
        {
            String [] possibleActions = world.getActions(state);
            String nextAction = possibleActions[rnd.nextInt(possibleActions.length)];
            if (!action.equals(nextAction)) {
                eligibility.initialize(0);
                action = nextAction;
            }
        }

        double [] nextState = world.applyAction(state, action);
        double reward = world.getReward(nextState);

        double delta = reward - features.getActivations(state, action).dot(theta);
        eligibility = eligibility.add(features.getActivations(state, action));

        if (world.isTerminal(nextState))
        {
            theta = theta.add(eligibility.mul(alpha*delta));
            return nextState;
        }

        action = getBestAction(nextState);
        delta = delta + gamma*(features.getActivations(nextState, action).dot(theta));
        theta = theta.add(eligibility.mul(alpha*delta));
        eligibility = eligibility.mul(gamma*lambda);

        return nextState;
    }

    public Vector qLearning(int numTrials, double epsilon, double epsilonGrowthRate)
    {
        for (int iteration = 0; iteration < numTrials; iteration++)
        {
            long startTime = System.currentTimeMillis();
            double [] state = world.getInitialState();

            while (!world.isTerminal(state))
            {
                state = episodeStep(state, epsilon);
            }


            epsilon *= epsilonGrowthRate;
            System.out.print(displayIter(iteration, numTrials, startTime));
        }

        return theta;
    }

    public Agent(World world, Features features, double alpha, double lambda, double gamma)
    {
        this.world = world;
        this.features = features;

        theta = features.getInitialParameters();
        eligibility = new Vector(theta);
        eligibility.initialize(0);

        this.alpha = alpha;
        this.lambda = lambda;
        this.gamma = gamma;
    }

    public World getWorld()
    {
        return world;
    }
}
