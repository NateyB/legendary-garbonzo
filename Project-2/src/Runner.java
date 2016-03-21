import java.util.Scanner;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-20.
 * <p>
 * This is the class that runs the Bayesian Network analysis
 */

public class Runner
{
    public static void main(String[] args)
    {
        int numSamples = 10000;
        EnumerationAsk enumAsk = new EnumerationAsk();
        LikelihoodWeighting likeWeight = new LikelihoodWeighting(numSamples);
        GibbsSampling gibbsSample = new GibbsSampling(numSamples);
        Scanner console = new Scanner(System.in);


        String read = "";
        while (true)
        {
            System.out.print("Please enter the file name (or stop or quit to exit): ");
            read = console.next();
            if (read.equals("stop") || read.equals("quit"))
            {
                return;
            }
            Dataset enumData = new Dataset(read);
            Dataset likeData = new Dataset(read);
            Dataset gibbsData = new Dataset(read);
            INode[] enumNetwork = enumData.getNetwork();
            INode[] likeNetwork = likeData.getNetwork();
            INode[] gibbsNetwork = gibbsData.getNetwork();


            while (true)
            {
                System.out.print("Please enter the id of the node that you wish to change (or anything else to continue): ");
                if (!console.hasNextInt())
                {
                    console.next();
                    break;
                }
                int id = console.nextInt();
                System.out.printf("Please enter the value you wish to set node %d: ", id);

                String val = console.next();
                enumNetwork[id].setValue(val);
                likeNetwork[id].setValue(val);
                gibbsNetwork[id].setValue(val);
            }

            while (true)
            {
                System.out.print("Please enter the id of the node that you wish to query (or anything else to continue): ");
                // TODO These networks aren't resetting properly after a query; therefore, only one variable can be appropriately queried per run. (Apparently restricted to the approximate algorithms).
                if (!console.hasNextInt())
                {
                    console.next();
                    break;
                }

                int id = console.nextInt();
                double[] enumOutput = enumAsk.query(enumNetwork[id], enumNetwork);
                double[] likeOutput = likeWeight.query(likeNetwork[id], likeNetwork);
                double[] gibbsOutput = gibbsSample.query(gibbsNetwork[id], gibbsNetwork);

                System.out.printf("Enumeration Ask: T: %f; F: %f%nLikelihood Weighting (%d samples): T: %f; F: %f%nGibbs' Sampling (%d samples): T: %f; F: %f%n%n",
                        enumOutput[0], enumOutput[1], numSamples, likeOutput[0], likeOutput[1], numSamples, gibbsOutput[0], gibbsOutput[1]);
            }
        }

    }
}
