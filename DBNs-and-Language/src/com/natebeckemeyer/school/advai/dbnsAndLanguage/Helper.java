package com.natebeckemeyer.school.advai.dbnsAndLanguage;

/**
 * Created for Projects by @author Nate Beckemeyer on 2016-05-02.
 */
public class Helper
{
    public static int getHashmapMaxCapacity(int numItems, double loadFactor)
    {
        return (int) Math.ceil(numItems / loadFactor);
    }

    public static int getHashmapMaxCapacity(int numItems)
    {
        return getHashmapMaxCapacity(numItems, .75);
    }
}
