package com.natebeckemeyer.school.advai.project2.nodes;

import java.util.ArrayList;
import java.util.Arrays;

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
        if (children == null)
        {
            children = childList.toArray(new INode[childList.size()]);
            childList = null;
        }
        return children;
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
     * @return Returns the probability distribution over each of the items in this nodes's domain (index
     * corresponding to
     * the index of getDomain()), given the item's parents.
     */
    @Override public double[] getDistribution()
    {
        int index = 0;
        int startSize = lookupTable.length;

        for (int i = parents.length - 1; i >= 0; i--)
        {
            startSize /= parents[i].getDomain().length;
            for (int j = 0; j < parents[i].getDomain().length; j++)
            {
                index*= Arrays.asList(parents[i].getDomain()).indexOf(parents[i].getValue());
            }
            /*index += (getParents()[i].getValue().equals("F") ? Math.pow(2,
                    i) : 0); // TODO Fix this to work with any discrete domains (eventually)*/
        }

        return lookupTable[index];
    }

    /**
     * @param distribution Sets the distribution of the node, even if the node is parentless or its probabilities
     *                     depend upon its parents.
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
