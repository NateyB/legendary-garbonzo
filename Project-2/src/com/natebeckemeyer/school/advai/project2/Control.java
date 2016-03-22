package com.natebeckemeyer.school.advai.project2;

import com.natebeckemeyer.school.advai.project2.algorithms.EnumerationAsk;
import com.natebeckemeyer.school.advai.project2.algorithms.GibbsSampling;
import com.natebeckemeyer.school.advai.project2.algorithms.LikelihoodWeighting;
import com.natebeckemeyer.school.advai.project2.nodes.INode;
import com.natebeckemeyer.school.advai.project2.utilities.Dataset;

import java.util.Scanner;

/**
 * Created for BayesianNetworks by @author Nate Beckemeyer on 2016-03-20.
 * <p>
 * This is the class that runs the Bayesian Network analysis.
 */

public class Control
{
    public static void main(String[] args)
    {
        int maximumSizeForExactInference = 30;
        int numSamples = 10000;



        EnumerationAsk enumAsk = new EnumerationAsk();
        LikelihoodWeighting likeWeight = new LikelihoodWeighting(numSamples);
        GibbsSampling gibbsSample = new GibbsSampling(numSamples);
        Scanner console = new Scanner(System.in);


        String read;
        while (true)
        {
            System.out.print("Please enter the file name (or stop or quit to exit): ");
            read = console.next();
            if (read.equals("stop") || read.equals("quit") || read.equals("exit"))
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
                System.out.print("Please enter the id of the nodes that you wish to change (or anything else to continue): ");
                if (!console.hasNextInt())
                {
                    console.next();
                    break;
                }
                int id = console.nextInt();
                System.out.printf("Please enter the value you wish to set nodes %d: ", id);

                String val = console.next();
                enumNetwork[id].setValue(val);
                likeNetwork[id].setValue(val);
                gibbsNetwork[id].setValue(val);
            }

            while (true)
            {
                System.out.print("Please enter the id of the nodes that you wish to query (or anything else to continue): ");
                if (!console.hasNextInt())
                {
                    console.next();
                    break;
                }

                int id = console.nextInt();
                long time = System.currentTimeMillis();

                double[] likeOutput = likeWeight.query(likeNetwork[id], likeNetwork);
                System.out.printf("%nLikelihood Weighting: T: %f F: %f; Time taken: %d", likeOutput[0], likeOutput[1], -time + (time = System.currentTimeMillis()));

                double[] gibbsOutput = gibbsSample.query(gibbsNetwork[id], gibbsNetwork);
                System.out.printf("%nGibbs Sampling: T: %f F: %f; Time taken: %d", gibbsOutput[0], gibbsOutput[1], -time + (time = System.currentTimeMillis()));

                if (enumNetwork.length <= maximumSizeForExactInference)
                {
                    double[] enumOutput = enumAsk.query(enumNetwork[id], enumNetwork);
                    System.out.printf("%nEnumeration Ask: T: %f F: %f; Time taken: %d", enumOutput[0], enumOutput[1], -time + System.currentTimeMillis());
                } else
                {
                    System.out.printf("%nUnfortunately, because the nodes has more than %d nodes, Enumeration Ask is intractable.", maximumSizeForExactInference);
                }

                System.out.printf("%n%n");
            }
        }

    }
}
