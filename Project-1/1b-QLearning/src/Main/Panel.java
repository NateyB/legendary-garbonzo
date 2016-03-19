package Main;

public class Panel
{
    private String type;
    private double reward;

    public String getType()
    {
        return type;
    }

    public double getReward()
    {
        return reward;
    }

    public Panel(String type, double reward)
    {
        this.type = type;
        this.reward = reward;
    }

    public Panel(String type)
    {
        this.type = type;
        this.reward = Main.defaultReward;
    }
}
