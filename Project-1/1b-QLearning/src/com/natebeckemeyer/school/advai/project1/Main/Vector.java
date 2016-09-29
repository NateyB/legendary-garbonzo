package com.natebeckemeyer.school.advai.project1.main;

import java.util.ArrayList;
import java.util.List;

public class Vector
{
    private ArrayList<Double> items = new ArrayList<>();


    private ArrayList<Double> getItems()
    {
        return items;
    }

    public void set(int where, double value)
    {
        while (where >= getItems().size())
        {
            getItems().add(0.);
        }
        getItems().set(where, value);
    }

    public void add(double value)
    {
        set(getItems().size(), value);
    }

    public int size()
    {
        return items.size();
    }

    public double dot(Vector other)
    {
        double sum = 0;
        for (int i = 0; i < items.size(); i++)
        {
            sum += getItems().get(i) * other.getItems().get(i);
        }

        return sum;
    }

    public Vector mul(Double other)
    {
        Vector newVec = new Vector(this);
        for (int i = 0; i < items.size(); i++)
        {
            newVec.getItems().set(i, other * items.get(i));
        }

        return newVec;
    }

    public Vector add(Vector other)
    {
        Vector newVec = new Vector(other);
        for (int i = 0; i < items.size(); i++)
        {
            newVec.getItems().set(i, items.get(i) + other.getItems().get(i));
        }

        return newVec;
    }

    public void initialize(double start)
    {
        for (int i = 0; i < getItems().size(); i++)
        {
            set(i, start);
        }
    }

    public Vector()
    {
    }

    public Vector(Vector other)
    {
        for (int i = 0; i < other.size(); i++)
        {
            getItems().add(i, other.getItems().get(i));
        }
    }

    public Vector(List<Double> vals)
    {
        items = new ArrayList<>(vals);
    }

    public String toString()
    {
        String retStr = "";
        for (int i = 0; i < getItems().size(); i++)
        {
            retStr += String.format("Item %d: %f%n", i, getItems().get(i));
        }

        return retStr;
    }
}
