package Node;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-20.
 */
public class BayesianNode implements INode
{
    private String value;
    private double [][] lookupTable;
    private String[] domain;
    private INode[] parents = new INode[0];

    public INode[] getParents()
    {
        return parents;
    }

    public void setParents(INode[] parents)
    {
        this.parents = parents;
    }

    /**
     * @return The truth value of the node (in the scope of a boolean project, either "T" or "F"). If the node's truth
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
     * @return An array of strings that contain the discrete values which this node can take (in a boolean project,
     * either "T" or "F").
     */
    @Override public String[] getDomain()
    {
        return domain;
    }

    /**
     * @return Returns the probability distribution over each of the items in this node's domain (index corresponding to
     * the index of getDomain()).
     */
    @Override public double[] getDistribution()
    {
        int index = 0;
        for (int i = 0; i < parents.length; i++)
        {
            index += (parents[i].getValue().equals("F") ? Math.pow(2,i) : 0); // TODO Fix this to work with any discrete domains (eventually)
        }

        return lookupTable[index];
    }

    /**
     * @param distribution Sets the distribution of the node, even if the node is parentless or its probabilities depend
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
