package com.natebeckemeyer.school.advai.project2.algorithms;

import com.natebeckemeyer.school.advai.project2.nodes.INode;
import com.natebeckemeyer.school.advai.project2.utilities.BayesianHelper;

import java.util.Arrays;
import java.util.List;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-20.
 */
public class EnumerationAsk implements IInferenceAlgorithm
{
    /**
     * Enumeration ask, as seen in the textbook (Russell & Norvig, 3rd Edition)
     *
     * @param query   The nodes that's being queried
     * @param network The array of nodes that form the Bayesian network
     * @return The probability distribution over the query variable, given the evidence and network
     */
    @Override public double[] query(INode query, INode[] network)
    {
        double[] distribution = new double[query.getDomain().length];
        if (query.getValue() != null)
        {
            distribution[Arrays.asList(query.getDomain()).indexOf(query.getValue())] = 1;
            return distribution;
        }
        for (int i = 0; i < distribution.length; i++)
        {
            query.setValue(query.getDomain()[i]);
            distribution[i] = enumerateAll(Arrays.asList(network));
        }

        query.setValue(null);
        return BayesianHelper.normalize(distribution);
    }

    /**
     * The enumerateAll component of the enumeration-ask algorithms
     *
     * @param vars The list of variables still being considered
     * @return The probability of those variables having their value
     */
    private double enumerateAll(List<INode> vars)
    {
        if (vars.size() == 0)
        {
            return 1;
        }

        INode y = vars.get(0);
        if (y.getValue() != null)
        {
            double prob = y.getDistribution()[Arrays.asList(y.getDomain()).indexOf(y.getValue())];
            return prob * enumerateAll(vars.subList(1, vars.size()));
        } else
        {
            double sum = 0;
            for (String val : y.getDomain())
            {
                y.setValue(val);
                double prob = y.getDistribution()[Arrays.asList(y.getDomain()).indexOf(y.getValue())];
                sum += prob * enumerateAll(vars.subList(1, vars.size()));
            }

            y.setValue(null);
            return sum;
        }

    }
}
