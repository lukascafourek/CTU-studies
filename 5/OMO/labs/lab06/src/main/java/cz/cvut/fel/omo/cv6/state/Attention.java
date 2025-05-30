package cz.cvut.fel.omo.cv6.state;

/**
 * Kurz A7B36OMO - Objektove modelovani - Cviceni 6 Design Patterns State, strategy
 *
 * @author mayerto1
 */
public class Attention extends State {

    public Attention(Context context) {
        super(context);
        color = Color.ORANGE;
        period = LightPeriod.ORANGE_LIGHT_PERIOD.getValue();
    }

    @Override
    protected void changeToNextState() {
        context.setState(new Stop(context));
    }
}
