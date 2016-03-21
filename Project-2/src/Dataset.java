import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-19.
 */
// TODO Generalize this class to work better with any domain, rather than this one alone (or create an interface)
public class Dataset
{
    private INode[] network;

    public INode[] getNetwork()
    {
        return network.clone();
    }

    public Dataset(String filePath)
    {
        ArrayList<INode> network = new ArrayList<>();
        Scanner parser = null;
        try
        {
            parser = new Scanner(new File(filePath));
        } catch (FileNotFoundException e)
        {
            e.printStackTrace();
        }
        while (parser.hasNext())
        {
            int id = parser.nextInt();
            ArrayList<Integer> parentList = new ArrayList<>();

            while (parser.hasNextInt())
            {
                parentList.add(parser.nextInt());
            }

            BayesianNode current = new BayesianNode(id, new String[]{"T", "F"});
            INode[] parents = new INode[parentList.size()];
            for (int i = 0; i < parentList.size(); i++)
            {
                parents[i] = network.get(parentList.get(i));
            }
            current.setParents(parents);
            if (parents.length > 0)
            {
                ArrayList<Double[]> distribution = new ArrayList<>();
                int totalValues = 1;
                for (INode parent : parents)
                {
                    totalValues *= parent.getDomain().length;
                }

                for (int j = 0; j < totalValues; j++)
                {
                    for (int i = 0; i < parents.length; i++)
                    {
                        if (!parser.hasNext())
                        {
                            System.err.println("Couldn't find parent's value in file!");
                        } else
                        {
                            parser.next();
                        }
                    }

                    double prob = parser.nextDouble();
                    distribution.add(new Double[]{prob, 1 - prob});
                }
                double[][] newDistribution = new double[distribution.size()][2];
                for (int i = 0; i < distribution.size(); i++)
                {
                    for (int j = 0; j < distribution.get(i).length; j++)
                    {
                        newDistribution[i][j] = distribution.get(i)[j];
                    }
                }

                current.setLookup(newDistribution);
            } else
            {
                double probability = parser.nextDouble();
                current.setLookup(new double[][]{{probability, 1 - probability}});
            }
            network.add(current);

        }
        INode[] newNetwork = new INode[network.size()];
        this.network = network.toArray(newNetwork);
    }
}
