package cz.cvut.fel.agents.pdv.pdv_cloud.vector;

import cz.cvut.fel.agents.pdv.dsand.DSProcess;
import cz.cvut.fel.agents.pdv.dsand.Message;
import cz.cvut.fel.agents.pdv.pdv_cloud.cloud_utils.Client;
import cz.cvut.fel.agents.pdv.pdv_cloud.cloud_utils.Database;
import cz.cvut.fel.agents.pdv.pdv_cloud.scalar.HighTrafficClient;
import cz.cvut.fel.agents.pdv.pdv_cloud.scalar.ScalarClock;

import java.util.Arrays;
import java.util.List;
import java.util.Queue;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Konfigurace simulace...
 */
public class VectorDSConfigBombarding implements cz.cvut.fel.agents.pdv.dsand.DSConfig {

    // prvky naseho cloudu - klienti a databaze
    private final List<String> clients = Stream.of("A1", "A2").collect(Collectors.toList());
    private final List<String> stores = Stream.of("S1", "S2").collect(Collectors.toList());
    private final List<String> bombarders = Arrays.asList("B");

    // vsechny procesy - klienti + databaze
    private final List<String> processes = Stream.concat(clients.stream(), Stream.concat(stores.stream(), bombarders.stream()))
            .collect(Collectors.toList());

    @Override
    public String[] getProcessIDs() {
        return processes.toArray(new String[processes.size()]);
    }

    @Override
    public DSProcess instantiateProcess(String id, Queue<Message> inbox,
                                        BiConsumer<String, Message> outbox, String[] directory) {
        if (clients.contains(id)) {
            // jedna se o klienta
            int myIndex = clients.indexOf(id), otherIndex = (myIndex + 1) % clients.size();
            return new Client<>(id, inbox, outbox,
                    new VectorClock(processes.size(), myIndex), 0.5,
                    clients.get(otherIndex), stores.get(myIndex), myIndex == 0);
        } else if (stores.contains(id)) {
            // jedna se o databazi
            int myIndex = stores.indexOf(id), otherIndex = (myIndex + 1) % stores.size();
            return new Database<>(id, inbox, outbox,
                    new VectorClock(processes.size(), myIndex + clients.size()), stores.get(otherIndex),
                    otherIndex);
        } else {
            return new HighTrafficClient("B", inbox, outbox, new VectorClock(processes.size(), 4), "S1");
        }
    }

    @Override
    public long getDeliveryDelay(String src, String dst) {

        // dorucovani se bude vzdy opozdovat mezi databazemi
//    if (stores.contains(src) && stores.contains(dst)){
//      return rnd.nextInt(3) + 1;
//    }

        return 1;
    }

    @Override
    public double getReliability(String src, String dst) {
        // zpravy se neztraceji
        return 1;
    }

    @Override
    public List<String> getProcessesToWake() {
        // zadne procesy nejsou ukonceny
        return processes;
    }

    @Override
    public boolean isRunning() {
        return true;
    }
}
