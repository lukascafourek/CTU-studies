package cz.cvut.fel.agents.pdv.pdv_cloud.vector;

import cz.cvut.fel.agents.pdv.pdv_cloud.cloud_utils.IClock;
import cz.cvut.fel.agents.pdv.pdv_cloud.cloud_utils.IProcessClock;

import java.util.Arrays;

/**
 * Trida reprezentujici vektorove hodiny procesu.
 */
public class VectorClock implements IProcessClock<int[]> {

    private final int[] vectorTime;

    // id procesu - vlastnika hodin
    private final int myID;

    public VectorClock(int processes, int myID) {
        this.vectorTime = new int[processes];
        this.myID = myID;
    }

    @Override
    public void update(IClock<int[]> timestamp) {
        for (int i = 0; i < vectorTime.length; i++) {
            vectorTime[i] = Math.max(vectorTime[i], timestamp.getTime()[i]);
        }
        vectorTime[myID]++;
    }

    @Override
    public void onNewEvent() {
        vectorTime[myID]++;
    }

    @Override
    public IClock<int[]> getAsTimeStamp() {
        return new VectorTimestamp(vectorTime.clone());
    }

    @Override
    public int[] getTime() {
        return vectorTime;
    }

    @Override
    public boolean isCausalityForProcessViolated(IClock<int[]> timestamp, int process) {
        return vectorTime[process] > timestamp.getTime()[process];
    }

    @Override
    public String toString() {
        return Arrays.toString(vectorTime);
    }
}
