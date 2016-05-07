package com.natebeckemeyer.school.advai.dbnsAndLanguage.language;

import com.natebeckemeyer.school.advai.dbnsAndLanguage.Helper;
import com.sun.istack.internal.NotNull;
import com.sun.istack.internal.Nullable;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * Created for Projects by @author Nate Beckemeyer on 2016-05-02.
 * <p>
 * This is a Trie where the keys are arbitrary objects.
 */
public class Trie<E>
{
    private E entry;
    private HashMap<E, Trie<E>> vocabulary;

    @NotNull
    public E getEntry()
    {
        return entry;
    }

    public void addEntry(E entry)
    {
        vocabulary.put(entry, new Trie<>(entry));
    }

    @Nullable
    public Trie<E> lookupEntry(E word)
    {
        return vocabulary.get(word);
    }

    @Nullable
    public List<E> getSequence(List<E> entries)
    {
        Trie<E> current = this;
        for (E entry : entries)
        {
            current = current.lookupEntry(entry);
            if (current == null)
                return null;
        }

        return entries;
    }

    public void addSequence(List<E> entries)
    {
        Trie<E> current = this;
        for (E entry : entries)
        {
            Trie<E> nextStep = new Trie<>(entry);
            current.vocabulary.put(entry, nextStep);
            current = nextStep;
        }
    }

    public Trie()
    {
        this.vocabulary = new HashMap<>();
    }

    public Trie(E entry)
    {
        this();
        this.entry = entry;
    }

    public Trie(Collection<E> vocabulary)
    {
        this.vocabulary = new HashMap<>(Helper.getHashmapMaxCapacity(vocabulary.size()));

        for (E entry : vocabulary)
            this.vocabulary.put(entry, new Trie<>(entry));
    }
}
