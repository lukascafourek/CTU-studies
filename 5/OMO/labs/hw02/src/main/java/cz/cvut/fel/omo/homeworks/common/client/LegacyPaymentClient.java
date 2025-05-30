package cz.cvut.fel.omo.homeworks.common.client;

import java.util.UUID;

/**
 * Can't touch this
 */
public class LegacyPaymentClient {

    /**
     * Executes transactions using old legacy API
     * @param payment payment information separated by semicolon
     * @param ip IP address of the sender
     * @return Unique ID of the transaction
     */
    public String execute(String payment, String ip) {
        System.out.printf("Executing transaction: %s from client IP: %s%n", payment, ip);

        // This only simulates the client

        return String.valueOf(UUID.randomUUID());
    }
}
