package com.natebeckemeyer.school.advai.dbnsAndLanguage.language;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Scanner;

/**
 * Created for Projects by @author Nate Beckemeyer on 2016-05-02.
 */
public class NGram
{
    /**
     * The N in NGram (the length of the sequence).
     */
    private int length;

    public NGram(int n)
    {
        length = n;
    }

    public File performAnalysis(File toExamine)
    {
        Trie vocabulary = new Trie();
        LinkedList<String> currentPhrase = new LinkedList<>();
        try
        {
            Scanner console = new Scanner(toExamine);
            for (int i = 0; i < 3 && console.hasNext(); i++)
            {
                currentPhrase.addLast(console.next());
            }
            while (console.hasNext())
            {
                currentPhrase.addLast(console.next());

                // TODO Damnit.
                if (vocabulary.getSequence(currentPhrase) != null)
                vocabulary.addSequence(currentPhrase);
                currentPhrase.removeFirst();
            }
        } catch (IOException error)
        {
            error.printStackTrace();
            System.exit(10);
        }
    }


}
