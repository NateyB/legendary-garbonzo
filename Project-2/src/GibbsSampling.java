import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-21.
 */
public class GibbsSampling implements IInferenceAlgorithm
{
    private int numSamples;
    private Random rnd = new Random();


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
     * @param query   The node that's being queried
     * @param network The array of nodes that form the Bayesian network
     * @return The probability distribution over the query variable, given the evidence and network
     */
    @Override public double[] query(INode query, INode[] network)
    {
        double [] counts = new double[query.getDomain().length];
        boolean [] evidence = new boolean[network.length];
        for (int i = 0; i < network.length; i++)
        {
            evidence[i] = network[i].getValue() != null;
        }


        for (int sample = 0; sample < numSamples; sample++)
        {
            for (int current = 0; current < network.length; current++)
            {
                if (!evidence[current])
                {
                    network[current].setValue(network[current].getDomain()[pickRandom(network[current].getDistribution())]); // TODO use whole Markov blanket
                    counts[Arrays.asList(network[current].getDomain()).indexOf(network[current].getValue())] += 1;
                }
            }
        }

        return normalize(counts);
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

    public GibbsSampling(int numSamples)
    {
        this.numSamples = numSamples;
    }

}
