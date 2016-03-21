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
        LikelihoodWeighting main = new LikelihoodWeighting(10000);
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
            Dataset current = new Dataset(read, main);
            INode[] network = current.getNetwork();


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
                network[id].setValue(console.next());
            }

            while (true)
            {
                System.out.print("Please enter the id of the node that you wish to query (or anything else to continue): ");
                if (!console.hasNextInt())
                {
                    console.next();
                    break;
                }
                int id = console.nextInt();
                double[] output = main.query(network[id], network);
                System.out.printf("%nT: %f; F: %f%n", output[0], output[1]);
            }
        }

    }
}
