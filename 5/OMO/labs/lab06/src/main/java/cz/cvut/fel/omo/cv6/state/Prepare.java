package cz.cvut.fel.omo.cv6.state;

// TODO - TO BE IMPLEMENTED

public class Prepare extends State {

    public Prepare(Context context) {
        super(context);
        color = Color.ORANGE;
        period = LightPeriod.ORANGE_LIGHT_PERIOD.getValue();
    }

    @Override
    protected void changeToNextState() {
        context.setState(new Go(context));
    }
}
