package Algorithms;

import Node.INode;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-20.
 */
public interface IInferenceAlgorithm
{
    /**
     * @param query The node that's being queried
     * @param network The array of nodes that form the Bayesian network
     * @return The probability distribution over the query variable, given the evidence and network
     */
    double [] query(INode query, INode [] network);
}
