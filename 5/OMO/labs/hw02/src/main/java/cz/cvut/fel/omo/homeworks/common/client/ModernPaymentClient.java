package cz.cvut.fel.omo.homeworks.common.client;

import java.util.UUID;

/**
 * Can't touch this
 */
public class ModernPaymentClient {

    /**
     * Executes transactions using new modern API
     *
     * @param transaction Transaction object serialized as JSON string
     *
     * @return Unique ID of the transaction
     */
    public String execute(String transaction) {
        System.out.printf("Executing transaction: %s %n", transaction);

        // This only simulates the client

        return String.valueOf(UUID.randomUUID());
    }
}
