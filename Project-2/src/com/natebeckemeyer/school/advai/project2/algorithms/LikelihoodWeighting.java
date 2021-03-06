package com.natebeckemeyer.school.advai.project2.algorithms;

import com.natebeckemeyer.school.advai.project2.nodes.INode;
import com.natebeckemeyer.school.advai.project2.utilities.BayesianHelper;

import java.util.Arrays;
import java.util.Random;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-21.
 *
 * The Likelihood Weighting algorithms for approximate inference in Bayesian networks
 */
public class LikelihoodWeighting implements IInferenceAlgorithm
{
    private int numSamples = -1;
    private Random rnd = new Random();

    public LikelihoodWeighting(int numSamples)
    {
        this.numSamples = numSamples;
    }

    /**
     * @param query   The nodes that's being queried
     * @param network The array of nodes that form the Bayesian network
     * @return The probability distribution over the query variable, given the evidence and network
     */
    @Override public double[] query(INode query, INode[] network)
    {
        double [] count = new double[query.getDomain().length];
        for (int j = 0; j < numSamples; j++)
        {
            EventWeightPair xW = weightedSample(network);
            boolean [] evidence = xW.getEvent();
            for (int i = 0; i < evidence.length; i++)
            {
                if (network[i] == query)
                {
                    count[Arrays.asList(query.getDomain()).indexOf(query.getValue())] += xW.getWeight();
                }
                if (!evidence[i])
                {
                    network[i].setValue(null);
                }
            }
        }

        return BayesianHelper.normalize(count);
    }

    private EventWeightPair weightedSample(INode[] network)
    {
        double weight = 1;
        boolean [] evidence = new boolean[network.length];

        for (int i = 0; i < network.length; i++)
        {
            INode current = network[i];
            if (current.getValue() != null)
            {
                weight *= current.getDistribution()[Arrays.asList(current.getDomain()).indexOf(current.getValue())];
                evidence[i] = true;
            } else
            {
                evidence[i] = false;
                current.setValue(current.getDomain()[pickRandom(current.getDistribution())]);
            }
        }

        return new EventWeightPair(weight, evidence);
    }

    private int pickRandom(double [] distribution)
    {
        double runningSum[] = new double[distribution.length];
        for (int i = 0; i < distribution.length; i++)
        {
            runningSum[i] = (i > 0) ? runningSum[i - 1] + distribution[i] : distribution[i];
        }
        double selection = rnd.nextDouble();

        for (int i = 0; i < runningSum.length; i++)
        {
            if (selection <= runningSum[i])
            {
                return i;
            }
        }

        return -1;
    }

    private class EventWeightPair
    {
        private boolean [] event;
        private double weight;

        boolean[] getEvent()
        {
            return event;
        }

        double getWeight()
        {
            return weight;
        }

        EventWeightPair(double weight, boolean[] event)
        {
            this.weight = weight;
            this.event = event;
        }
    }
}
