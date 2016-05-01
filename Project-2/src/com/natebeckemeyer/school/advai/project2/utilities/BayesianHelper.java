package com.natebeckemeyer.school.advai.project2.utilities;

/**
 * Created for CS-7313-Adv-Artificial-Intelligence by @author Nate Beckemeyer on 2016-03-27.
 */
public class BayesianHelper
{
    /**
     * @param dist The distribution that will be normalized (without being muted)
     * @return A normalized distribution based on dist (that is, it sums to 1)
     */
    public static double [] normalize(double[] dist)
    {
        double sum = 0;
        for (double x : dist)
        {
            sum += x;
        }
        double alpha = 1 / sum;

        double [] newDist = new double[dist.length];
        for (int i = 0; i < dist.length; i++)
        {
            newDist[i] = dist[i]*alpha;
        }

        return newDist;
    }
}
