package com.natebeckemeyer.school.advai.project2.algorithms;

import com.natebeckemeyer.school.advai.project2.nodes.INode;
import com.natebeckemeyer.school.advai.project2.utilities.BayesianHelper;

import java.util.Arrays;
import java.util.Random;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-21.
 * <p>
 * Note that this is equivalent to the Monte-Carlo Markov Chain Ask as presented in the slides.
 */
public class GibbsSampling implements IInferenceAlgorithm
{
    private int numSamples;
    private Random rnd = new Random();

    /**
     * @param node The node whose value needs a corresponding index
     * @return The index of the node's current value in the array of the node's domain.
     */
    private int properIndexOfValue(INode node)
    {
        return Arrays.asList(node.getDomain()).indexOf(node.getValue());
    }

    /**
     * Returns the probability that a node's value is what it's been (temporarily) assigned to be, given its Markov
     * blanket.
     *
     * @param node The node to be tested
     * @return The probability that node's value is what it's been assigned to be given its Markov blanket.
     */
    private double markovBlanketProbability(INode node)
    {
        double probability = node.getDistribution()[properIndexOfValue(node)];
        INode[] children = node.getChildren();

        for (INode child : children)
            probability *= child.getDistribution()[properIndexOfValue(child)];

        return probability;
    }

    /**
     * @param query   The node that's being queried
     * @param network The array of nodes that form the Bayesian network
     * @return The probability distribution over the query variable, given the evidence and network
     */
    @Override public double[] query(INode query, INode[] network)
    {
        double[] counts = new double[query.getDomain().length];
        boolean[] evidence = new boolean[network.length];
        for (int i = 0; i < network.length; i++)
        {
            evidence[i] = network[i].getValue() != null;
            network[i].setValue(
                    network[i].getValue() != null ? network[i].getValue() : network[i].getDomain()[pickRandom(
                            network[i].getDistribution())]);
        }


        for (int sample = 0; sample < numSamples; sample++)
        {
            for (int current = 0; current < network.length; current++)
            {
                if (!evidence[current])
                {
                    double[] distribution = new double[query.getDomain().length];
                    for (int i = 0; i < query.getDomain().length; i++)
                    {
                        // Calculating the markov blanket probability for each value of the node
                        network[current].setValue(query.getDomain()[i]);
                        distribution[i] = markovBlanketProbability(network[current]);
                    }
                    network[current].setValue(network[current].getDomain()[pickRandom(BayesianHelper.normalize(distribution))]);
                    counts[properIndexOfValue(query)] += 1;
                }
            }
        }

        for (int i = 0; i < evidence.length; i++)
        {
            if (!evidence[i])
            {
                network[i].setValue(null);
            }
        }

        return BayesianHelper.normalize(counts);
    }

    /**
     * Picks a random index of the probability distribution, proportional to their probabilities.
     *
     * @param distribution The probability distribution needing an item selected
     * @return An integer indicating which item in the probability distribution was chosen
     */
    private int pickRandom(double[] distribution)
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

        return -2;
    }

    public GibbsSampling(int numSamples)
    {
        this.numSamples = numSamples;
    }

}
