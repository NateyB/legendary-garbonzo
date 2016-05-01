package com.natebeckemeyer.school.advai.project1.Main;

import com.natebeckemeyer.school.advai.project1.Exercise21A.*;
import com.natebeckemeyer.school.advai.project1.Exercise21B.*;
import com.natebeckemeyer.school.advai.project1.RussellPackage.*;

// Gaussian offset for both components of movement vector

public class Main
{
    public static final double defaultReward = -.04;
    public static final double epsilonGrowthRate = .999;
    private static final int numTrials = 5000;
    private static final double epsilon = 1;

    private static void displayPolicy(Agent learner)
    {
        String policy = learner.getWorld().getPolicy(learner);
        System.out.println(policy);
    }

    public static void main(String[] args)
    {
        RussellWorld russellWorld = new RussellWorld();
        Exercise21AWorld exercise21AWorld = new Exercise21AWorld();
        Exercise21BWorld exercise21BWorld = new Exercise21BWorld();

        // Radial
        Agent codyBanks = new Agent(russellWorld, new RussellRadial(), .005, 1, 1);
        Agent samBeckmann = new Agent(exercise21AWorld, new Exercise21ARadial(), .0005, 1, 1);
        Agent doctorSen = new Agent(exercise21BWorld, new Exercise21BRadial(), .0001, 1, 1);

        // Tiling
        Agent johnnyEnglish = new Agent(russellWorld, new RussellTiling(), .005, 1, 1);
        Agent nateBeckemeyer = new Agent(exercise21AWorld, new Exercise21ATiling(), .002, 1, 1);
        Agent doctorWainwright = new Agent(exercise21BWorld, new Exercise21BTiling(), .002, 1, 1);

//        Agent[] testAgents = new Agent [] {codyBanks, samBeckmann, doctorSen, johnnyEnglish, nateBeckemeyer, doctorWainwright};
//        int [] testTrials = new int [] {1000, 10000, 10000, 1000, 10000, 10000};

//        Agent[] testAgents = new Agent [] {samBeckmann, doctorSen, johnnyEnglish, nateBeckemeyer, doctorWainwright};
 //       int [] testTrials = new int [] {10000, 10000, 10000, 10000, 10000};

        Agent[] testAgents = new Agent [] {codyBanks, johnnyEnglish};
        int [] testTrials = new int [] {10000, 3000};


        for (int curAgent = 0; curAgent < testAgents.length; curAgent++)
        {
            Agent activeAgent = testAgents[curAgent];
            System.out.println("Testing new agent: \n");
            activeAgent.qLearning(testTrials[curAgent], epsilon, epsilonGrowthRate);
            displayPolicy(activeAgent);
            System.out.println();
            System.out.println();
        }
    }
}
