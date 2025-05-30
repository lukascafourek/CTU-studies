package cz.cvut.fel.omo.homeworks.refactor.transaction;

import cz.cvut.fel.omo.homeworks.common.client.LegacyPaymentClient;
import cz.cvut.fel.omo.homeworks.common.session.UserSession;
import java.util.Objects;
import java.util.Optional;

public class LegacyTransactionSystem extends AbstractTransactionSystem {

    private final UserSession userSession = new UserSession();
    private final LegacyPaymentClient legacyPaymentClient = new LegacyPaymentClient();

    public LegacyTransactionSystem(Long totalAmount, String currencyCode) {
        super(totalAmount, currencyCode);
    }

    @Override
    public Optional<String> execute(String transaction) {
        if (Objects.equals(transaction, "failed")) {
            return Optional.empty();
        }
        String ip = userSession.getIP();

        // This only simulates the client

        String uuid = legacyPaymentClient.execute(transaction, ip);
        return Optional.of(uuid);
    }

    @Override
    public Optional<String> buildTransaction() {
        if (currencyCode == null || totalAmount == null) {
            return Optional.empty();
        }
        return Optional.of(String.join(";", currencyCode, totalAmount.toString()));
    }
}
