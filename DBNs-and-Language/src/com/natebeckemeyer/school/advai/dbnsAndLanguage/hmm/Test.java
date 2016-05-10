package com.natebeckemeyer.school.advai.dbnsAndLanguage.hmm;

import org.ejml.simple.SimpleMatrix;

import java.util.LinkedList;

/**
 * Created for Projects by @author Nate Beckemeyer on 2016-05-07.
 */
public class Test
{
    public static void main(String[] args)
    {
        // {P(enough sleep), P(!enough sleep)}
        SimpleMatrix sleepProbs  = new SimpleMatrix(new double[][]{{.7, .3}});

        // The diagonal is the probability of the state's not changing from one timestep to another; the rows sum to 1.
        SimpleMatrix transitions = new SimpleMatrix(new double[][]{
                {.8, .3},
                {.2, .7}
        });

        // The observation matrices; for each e_t, the corresponding matrix (where the 0th row means enough sleep)
        SimpleMatrix [] observations = new SimpleMatrix[] {new SimpleMatrix(new double[][]{
                {.72, 0},
                {0, .21}
        }), new SimpleMatrix(new double[][]{
                {.18, 0},
                {0, .49}
        }), new SimpleMatrix(new double[][]{
                {.08, 0},
                {0, .09}
        }), new SimpleMatrix(new double[][]{
                {.02, 0},
                {0, .21}
        })};


        HiddenMarkovModel sleepModel = new HiddenMarkovModel(transitions, observations, sleepProbs);

        // Testing begins
        int [] observationSequence = {0,1,3};
        int [] evidence = new int[25];

        for (int count = 0; count < 25; count++)
        {
            evidence[count] = observationSequence[(Math.floorMod(count, 3))];
        }

        LinkedList<SimpleMatrix> fbSmoothed = sleepModel.forwardBackward(evidence);
        LinkedList<SimpleMatrix> cdSmoothed = sleepModel.countryDance(evidence);
        LinkedList<SimpleMatrix> fl2Smoothed = sleepModel.fixedLagSmoothing(evidence, 2);
        LinkedList<SimpleMatrix> fl3Smoothed = sleepModel.fixedLagSmoothing(evidence, 3);
        LinkedList<SimpleMatrix> fl4Smoothed = sleepModel.fixedLagSmoothing(evidence, 4);
        LinkedList<SimpleMatrix> fl5Smoothed = sleepModel.fixedLagSmoothing(evidence, 5);
        LinkedList<SimpleMatrix> pfSmoothed = sleepModel.particleFiltering(evidence, 200);

        System.out.printf("%-8s\t%-8s\t%-8s\t%-8s\t%-8s\t%-8s\t%-8s%n", "FB", "CD", "FL2", "FL3", "FL4", "FL5", "PF");
        for (int i = 0; i < evidence.length; i++)
        {
            System.out.printf("%.6f\t%.6f\t%.6f\t%.6f\t%.6f\t%.6f\t%.6f%n", fbSmoothed.get(i).get(0), cdSmoothed.get(i).get(0), fl2Smoothed.get(i).get(0), fl3Smoothed.get(i).get(0), fl4Smoothed.get(i).get(0), fl5Smoothed.get(i).get(0), pfSmoothed.get(i).get(0));
        }
    }
}
