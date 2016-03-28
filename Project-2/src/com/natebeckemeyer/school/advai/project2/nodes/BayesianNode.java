package com.natebeckemeyer.school.advai.project2.nodes;

import java.util.ArrayList;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-20.
 */
public class BayesianNode implements INode
{
    private String value;
    private double[][] lookupTable;
    private String[] domain;

    private INode[] parents = new INode[0];
    private ArrayList<INode> childList = new ArrayList<>(0);
    private INode[] children;


    public INode[] getParents()
    {
        return parents;
    }

    public INode[] getChildren()
    {
        return (children == null ? children = childList.toArray(new INode[childList.size()]) : children);
    }

    public void addChild(INode child)
    {
        childList.add(child);
    }

    public void setParents(INode[] parents)
    {
        this.parents = parents;
    }

    /**
     * @return The truth value of the nodes (in the scope of a boolean project, either "T" or "F"). If the nodes's truth
     * value is unknown (it is a hidden or query variable), then it returns null.
     */
    @Override public String getValue()
    {
        return value;
    }

    public void setValue(String value)
    {
        this.value = value;
    }

    /**
     * @return An array of strings that contain the discrete values which this nodes can take (in a boolean project,
     * either "T" or "F").
     */
    @Override public String[] getDomain()
    {
        return domain;
    }

    /**
     * @return Returns the probability distribution over each of the items in this nodes's domain (index corresponding to
     * the index of getDomain()).
     */
    @Override public double[] getDistribution()
    {
        int index = 0;
        for (int i = 0; i < getParents().length; i++)
        {
            index += (getParents()[i].getValue().equals("F") ? Math.pow(2,
                    i) : 0); // TODO Fix this to work with any discrete domains (eventually)
        }

        return lookupTable[index];
    }

    /**
     * @param distribution Sets the distribution of the nodes, even if the nodes is parentless or its probabilities depend
     *                     upon its parents.
     */
    @Override public void setLookup(double[][] distribution)
    {
        this.lookupTable = distribution;
    }

    public BayesianNode(String[] domain)
    {
        this.domain = domain;
    }
}
