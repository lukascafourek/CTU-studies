package cz.cvut.fel.agents.pdv.swim;

import cz.cvut.fel.agents.pdv.dsand.Message;
import cz.cvut.fel.agents.pdv.dsand.Pair;

import java.util.*;

/**
 * Trida s implementaci metody act() pro proces Failure Detector. Tuto tridu (a tridy pouzivanych zprav) budete
 * odevzdavat. Do tridy si muzete doplnit vlastni pomocne datove struktury. Hodnoty muzete inicializovat primo
 * v konstruktoru. Klicova je metoda act(), kterou vola kazda instance tridy FailureDetectorProcess ve sve metode
 * act(). Tuto metodu naimplementujte podle protokolu SWIM predstaveneho na prednasce.
 *
 * Pokud si stale jeste nevite rady s frameworkem, inspiraci muzete nalezt v resenych prikladech ze cviceni.
 */
public class ActStrategy {

    private final int maxDelayForMessages;
    private final List<String> otherProcesses;
    private final int timeToDetectKilledProcess;
    private final int upperBoundOnMessages;
    private int wakecount = 0;
    Map<String, Integer> processTimes;
    Set<String> susProcesses;
    Set<String> deadProcesses;
    Map<String, String> checks;
    private final Random random = new Random();

    public ActStrategy(int maxDelayForMessages, List<String> otherProcesses,
                       int timeToDetectKilledProcess, int upperBoundOnMessages) {
        this.maxDelayForMessages = maxDelayForMessages;
        this.otherProcesses = otherProcesses;
        this.timeToDetectKilledProcess = timeToDetectKilledProcess;
        this.upperBoundOnMessages = upperBoundOnMessages;
        processTimes = new HashMap<>();
        deadProcesses = new HashSet<>();
        susProcesses = new HashSet<>();
        checks = new HashMap<>();
    }

    /**
     * Metoda je volana s kazdym zavolanim metody act v FailureDetectorProcess. Metodu implementujte
     * tak, jako byste implementovali metodu act() v FailureDetectorProcess, misto pouzivani send()
     * pridejte zpravy v podobe paru - prijemce, zprava do listu. Zpravy budou nasledne odeslany.
     * <p>
     * Diky zavedeni teto metody muzeme kontrolovat pocet odeslanych zprav vasi implementaci.
     */
    public List<Pair<String, Message>> act(Queue<Message> inbox, String disseminationProcess) {
        List<Pair<String, Message>> out = new ArrayList<>();
        String firstProcess = otherProcesses.get(random.nextInt(otherProcesses.size()));
        if (!deadProcesses.contains(firstProcess) && wakecount < upperBoundOnMessages/10) {
            processTimes.put(firstProcess, wakecount);
            out.add(new Pair<>(firstProcess, new PingMessage()));
        }
        while (!inbox.isEmpty()) {
            Message message = inbox.poll();
            if (message instanceof PingMessage) {
                out.add(new Pair<>(message.sender, new AckMessage()));
            } else if (message instanceof AckMessage) {
                processTimes.remove(message.sender);
                if (checks.containsKey(message.sender)) {
                    out.add(new Pair<>(checks.get(message.sender), new ConfirmMessage(message.sender)));
                    checks.remove(message.sender);
                }
            } else if (message instanceof DeadProcessMessage) {
                String process = ((DeadProcessMessage) message).getProcessID();
                checks.remove(process);
                processTimes.remove(process);
                deadProcesses.add(process);
            } else if (message instanceof CheckMessage) {
                processTimes.remove(message.sender);
                String process = ((CheckMessage) message).getProcessID();
                checks.put(process, message.sender);
                out.add(new Pair<>(process, new PingMessage()));
            } else if (message instanceof ConfirmMessage) {
                String process = ((ConfirmMessage) message).getProcessID();
                susProcesses.remove(process);
                processTimes.remove(process);
                processTimes.remove(message.sender);
            }
        }
        for (String process : otherProcesses) {
            if (!deadProcesses.contains(process) && processTimes.containsKey(process)) {
                if (processTimes.get(process) < wakecount - (timeToDetectKilledProcess - maxDelayForMessages)/2) {
                    if (!susProcesses.contains(process)) {
                        for (int i = 0; i < 10; i++) {
                            String processID = otherProcesses.get(random.nextInt(otherProcesses.size()));
                            out.add(new Pair<>(processID, new CheckMessage(process)));
                        }
                        susProcesses.add(process);
                    }
                    if (processTimes.get(process) < wakecount - timeToDetectKilledProcess + maxDelayForMessages) {
                        if (!deadProcesses.contains(process) && susProcesses.contains(process)) {
                            out.add(new Pair<>(disseminationProcess, new PFDMessage(process)));
                        }
                    }
                }
            }
            if (deadProcesses.contains(process) && !out.isEmpty()) {
                out.removeLast();
            }
        }
        wakecount++;
        return out;
    }
}
