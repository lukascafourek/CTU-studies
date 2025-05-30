package cz.cvut.fel.omo.cv6.state;

// TODO - TO BE IMPLEMENTED

public class Stop extends State {

    public Stop(Context context) {
        super(context);
        color = Color.RED;
        period = LightPeriod.RED_LIGHT_PERIOD.getValue();
    }

    @Override
    protected void changeToNextState() {
        context.setState(new Prepare(context));
    }
}