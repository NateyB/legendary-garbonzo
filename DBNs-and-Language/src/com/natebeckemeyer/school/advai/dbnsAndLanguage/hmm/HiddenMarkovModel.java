package com.natebeckemeyer.school.advai.dbnsAndLanguage.hmm;

import org.ejml.simple.SimpleMatrix;

import java.util.LinkedList;
import java.util.Random;

/**
 * Created for Projects by @author Nate Beckemeyer on 2016-05-07.
 * <p>
 * This class contains the implementations of a Hidden Markov Model.
 */
public class HiddenMarkovModel
{
    /**
     * Used for random generation in the sampling methods.
     */
    private Random rnd = new Random();

    /**
     * The initial state
     */
    private final SimpleMatrix initial;

    /**
     * The transition model, T, an SxS matrix where T_{ij} = P(X_t = j|X_{t-1} = i)
     */
    private final SimpleMatrix transition;

    /**
     * The sensor matrices, SxS diagonal matrices, O_t, whose ith diagonal entry is P(e_t|X_t = i)
     * and whose other entries are 0.
     */
    private final SimpleMatrix[] observationMatrices;


    public HiddenMarkovModel(SimpleMatrix stateTransitionModel, SimpleMatrix[] observationProbabilityMatrices,
                             SimpleMatrix initialStateDistribution)
    {
        transition = stateTransitionModel;
        observationMatrices = observationProbabilityMatrices;
        initial = initialStateDistribution.transpose();

    }

    /**
     * Generates a linked list of the forward vectors for the algorithms implemented here
     * @param evidenceVariables The evidence used in the forwarding
     * @return Linked list of column vectors containing the probability state at each timestep
     */
    private LinkedList<SimpleMatrix> generateForward(int[] evidenceVariables)
    {
        LinkedList<SimpleMatrix> forwardColumn = new LinkedList<>();
        forwardColumn.addLast(initial);

        // Perform forward
        for (int evidence : evidenceVariables)
        {
            SimpleMatrix sensorUpdate = observationMatrices[evidence];
            forwardColumn.addLast(
                    normalizeColumns(sensorUpdate.mult(transition.transpose().mult(forwardColumn.getLast()))));
        }

        return forwardColumn;
    }

    /**
     * Generates a column vector of ones.
     *
     * @param rows The number of rows in the vector.
     * @return A column vector of ones.
     */
    private SimpleMatrix onesColumn(int rows)
    {
        double[][] values = new double[rows][1];
        for (int i = 0; i < rows; i++)
        {
            values[i][0] = 1;
        }

        return new SimpleMatrix(values);
    }

    /**
     * Performs the random sampling for particle filtering
     * @param state The current state
     * @param init Is this the initialization sampling?
     * @return The next state (determined by the sampling)
     */
    private int sample(int state, boolean init)
    {
        SimpleMatrix choices;
        if (init)
            choices = initial;
        else
            choices = transition.extractVector(false, state);


        double val = rnd.nextDouble();
        double sum = 0;

        for (int i = 0; i < choices.numRows(); i++)
        {
            sum += choices.get(i);
            if (val <= sum)
                return i;
        }

        return -1;
    }

    private int sample(int state)
    {
        return sample(state, false);
    }

    /**
     * Performs particle filtering on the HMM (our representation of a DBN).
     * @param evidenceVariables Evidence to use in analysis
     * @param samples Number of samples
     * @return A smoothed set of nodes over time
     */
    public LinkedList<SimpleMatrix> particleFiltering(int[] evidenceVariables, int samples)
    {
        LinkedList<SimpleMatrix> smoothed = new LinkedList<>();
        int[] stateVector = new int[samples + 1];
        stateVector[0] = sample(0, true);
        // TODO stateVector is larger than weights

        for (int time = 1; time <= evidenceVariables.length; time++)
        {
            double[] weights = new double[stateVector.length];

            for (int i = 1; i <= samples; i++)
            {
                stateVector[i] = sample(stateVector[i - 1]);
                SimpleMatrix sensorMatrix = observationMatrices[evidenceVariables[time - 1]];
                weights[i - 1] = (sensorMatrix.get(stateVector[i], stateVector[i]));
            }

            weights = normalize(weights);

            double val = rnd.nextDouble();
            double sum = 0;

            int count = 1;
            while (count < stateVector.length)
            {
                for (int i = 0; i < samples; i++)
                {
                    sum += weights[i];

                    if (val < sum)
                        stateVector[count] = stateVector[i + 1];
                }
                count++;
            }

            double proportion = 0;
            for (int i = 0; i < samples; i++)
            {
                proportion += (stateVector[i] == 0) ? 1 : 0;
            }
            proportion /= samples;
            smoothed.add(new SimpleMatrix(new double[][]
                    {
                            {proportion},
                            {1 - proportion}
                    }));
        }

        return smoothed;
    }

    /**
     * Normalizes an array of doubles so that they sum to 1
     * @param initial The array
     * @return A proportional probability distribution
     */
    private double[] normalize(double[] initial)
    {
        double sum = 0;
        for (double weight : initial)
        {
            sum += weight;
        }
        sum = (sum != 0 ? sum : 1);

        double[] normalized = new double[initial.length];

        for (int i = 0; i < normalized.length; i++)
        {
            normalized[i] = initial[i] / sum;
        }

        return normalized;
    }

    /**
     * The forward-backward algorithm implementation
     * @param evidenceVariables
     * @return
     */
    public LinkedList<SimpleMatrix> forwardBackward(int[] evidenceVariables)
    {
        LinkedList<SimpleMatrix> forwardColumn = generateForward(evidenceVariables);
        SimpleMatrix backward = onesColumn(forwardColumn.getFirst().numRows());

        LinkedList<SimpleMatrix> smoothedColumn = new LinkedList<>();
        smoothedColumn.addLast(forwardColumn.removeLast());

        // Perform backward
        int count = evidenceVariables.length;
        while (count-- > 0)
        {
            SimpleMatrix sensorMatrix = observationMatrices[evidenceVariables[count]];

            smoothedColumn.addFirst(normalizeColumns(forwardColumn.removeLast().elementMult(backward)));
            backward = normalizeColumns(transition.mult(sensorMatrix.mult(backward)));
        }

        return smoothedColumn;
    }

    /**
     * The Coustry Dance algorithm implementation
     * @param evidenceVariables
     * @return
     */
    public LinkedList<SimpleMatrix> countryDance(int[] evidenceVariables)
    {
        // Yeah, I technically generate a linked list because that's what generateForward does,
        // but I dump all of the intermediate results!
        SimpleMatrix forward = generateForward(evidenceVariables).getLast();
        LinkedList<SimpleMatrix> smoothedColumn = new LinkedList<>();

        // Perform backward
        SimpleMatrix backward = onesColumn(forward.numRows());


        for (int count = evidenceVariables.length - 1; count >= 0; count--)
        {
            SimpleMatrix tTransposeInverse = transition.transpose().invert();
            SimpleMatrix observedInverse = observationMatrices[evidenceVariables[count]].invert();

            forward = normalizeColumns(tTransposeInverse.mult(observedInverse).mult(forward));
            smoothedColumn.addFirst(normalizeColumns(forward.elementMult(backward)));

            backward = normalizeColumns(transition.mult(observationMatrices[evidenceVariables[count]].mult(backward)));
        }

        return smoothedColumn;
    }

    /**
     * The fixed-lag smoothing implementation
     * @param evidenceVariables
     * @param lag The amount of lag to wait
     * @return
     */
    public LinkedList<SimpleMatrix> fixedLagSmoothing(int[] evidenceVariables, int lag)
    {
        // Initialize persistents
        int time = 1;
        SimpleMatrix forward = initial.copy();
        SimpleMatrix lagBackwardMatrix = SimpleMatrix.identity(transition.numCols());
        LinkedList<Integer> evidenceLag = new LinkedList<>();

        LinkedList<SimpleMatrix> smoothedColumn = new LinkedList<>();
        SimpleMatrix next;

        while (time <= evidenceVariables.length)
        {
            // Begin algorithm
            int evidence = evidenceVariables[time - 1];
            evidenceLag.addLast(evidence);
            SimpleMatrix sensorMatrix = observationMatrices[evidence];

            if (time > lag)
            {
                forward = normalizeColumns(sensorMatrix.mult(transition.transpose().mult(forward))); // √
                int evLag = evidenceLag.removeFirst(); // ?
                SimpleMatrix sensorMatrixLag = normalizeColumns(observationMatrices[evLag]); //?
                lagBackwardMatrix = normalizeRows((sensorMatrixLag.invert()).mult(transition.invert()).mult(
                        lagBackwardMatrix).mult(
                        transition).mult(sensorMatrix));
            } else
                lagBackwardMatrix = lagBackwardMatrix.mult(transition.mult(sensorMatrix));

            time++;

            if (time > lag + 1)
            {
                SimpleMatrix b1 = normalizeColumns(lagBackwardMatrix.mult(onesColumn(lagBackwardMatrix.numCols())));
                next = normalizeColumns(forward.elementMult(b1));
            } else
                next = new SimpleMatrix(new double[][]{{-1}, {-1}});

            smoothedColumn.addFirst(next);
        }

        return smoothedColumn;
    }

    /**
     * Normalizes the rows of a matrix
     * @param toNormalize The matrix whose rows are being normalized
     * @return
     */
    public static SimpleMatrix normalizeRows(SimpleMatrix toNormalize)
    {
        SimpleMatrix newM = toNormalize.copy();
        for (int row = 0; row < newM.numRows(); row++)
        {
            SimpleMatrix curRow = newM.extractVector(true, row);
            double sum = curRow.elementSum();
            curRow = curRow.scale((sum != 0) ? 1 / sum : 1);

            for (int col = 0; col < curRow.numCols(); col++)
            {
                newM.set(row, col, curRow.get(col));
            }
        }
        return newM;
    }

    /**
     * Normalizes the columns of a matrix
     * @param toNormalize
     * @return
     */
    public static SimpleMatrix normalizeColumns(SimpleMatrix toNormalize)
    {
        return HiddenMarkovModel.normalizeRows(toNormalize.transpose()).transpose();
    }

}
