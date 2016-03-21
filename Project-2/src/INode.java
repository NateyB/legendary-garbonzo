import java.util.Collection;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-19.
 */

public interface INode
{
    /**
     * @return The value of the node (in the scope of a boolean project, either "T" or "F") if it is evidence.
     * If the node's value is unknown (it is a hidden or query variable), then return null.
     */
    String getValue();

    /**
     * @param value The value that getValue should return when called.
     */
    void setValue(String value);

    /**
     * @param parents The nodes which function as parent nodes to the entity; returned by getParents().
     */
    void setParents(INode [] parents);

    /**
     * @return The parents of the node
     */
    INode [] getParents();

    /**
     * @return An array of strings that contain the discrete values which this node can take (in a boolean project,
     * either "T" or "F").
     */
    String [] getDomain();

    /**
     *
     * @return Returns the distribution over this node's domain, given its parents
     */
    double [] getDistribution();

    /**
     * @param distribution Sets the lookup table of the node. Each row i is a distribution over the node's domain
     *                     as in row i of the lookup table (that is, parental configuration i).
     */
    void setLookup(double [][] distribution);
}
