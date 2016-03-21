import java.util.Arrays;
import java.util.List;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-20.
 */
public class EnumerationAsk implements IInferenceAlgorithm
{
    /**
     * @param dist A non-normalized distribution (may or may not sum to 1)
     * @return A normalized probability distribution (sums to 1)
     */
    private double[] normalize(double[] dist)
    {
        double sum = 0;
        for (double x : dist)
        {
            sum += x;
        }
        double alpha = 1 / sum;
        for (int i = 0; i < dist.length; i++)
        {
            dist[i] *= alpha;
        }
        return dist;
    }

    /**
     * Enumeration ask, as seen in the textbook (Russell & Norvig, 3rd Edition)
     *
     * @param query   The node that's being queried
     * @param network The array of nodes that form the Bayesian network
     * @return The probability distribution over the query variable, given the evidence and network
     */
    @Override public double[] query(INode query, INode[] network)
    {
        double[] distribution = new double[query.getDomain().length];
        for (int i = 0; i < distribution.length; i++)
        {
            query.setValue(query.getDomain()[i]);
            distribution[i] = enumerateAll(Arrays.asList(network));
        }

        query.setValue(null);
        return normalize(distribution);
    }

    /**
     * The enumerateAll component of the enumeration-ask algorithm
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
