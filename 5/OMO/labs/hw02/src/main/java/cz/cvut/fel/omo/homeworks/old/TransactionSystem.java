package cz.cvut.fel.omo.homeworks.old;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.fel.omo.homeworks.common.client.LegacyPaymentClient;
import cz.cvut.fel.omo.homeworks.common.client.ModernPaymentClient;
import cz.cvut.fel.omo.homeworks.common.model.ModernTransaction;
import cz.cvut.fel.omo.homeworks.common.session.UserSession;

import java.util.UUID;

public class TransactionSystem {

    private static final String TRANSACTION_FAILURE = "Transaction execution failed.";
    private final ObjectMapper objectMapper = new ObjectMapper();
    private final UserSession userSession = new UserSession(); // this is needed for legacy system only

    /**
     * Legacy system client
     */
    private final LegacyPaymentClient legacyPaymentClient = new LegacyPaymentClient();

    /**
     * Modern system client
     */
    private final ModernPaymentClient modernPaymentClient = new ModernPaymentClient();

    private boolean useModernGateway = true; // this only tells if we run the request under Modern payment system

    private Long totalAmount;
    private String currencyCode;

    private String surl; // this is needed for modern system only
    private String furl; // this is needed for modern system only

    /**
     * Use this method prepare attributes for execution using the <b>legacy</b> transaction system
     *
     * @param totalAmount  amount for transfer
     * @param currencyCode current for transfer
     */
    public void setTransactionParams(Long totalAmount, String currencyCode) {
        this.useModernGateway = false;
        this.totalAmount = totalAmount;
        this.currencyCode = currencyCode;
    }

    /**
     * Use this method prepare attributes for execution using the <b>modern</b> transaction system
     *
     * @param totalAmount  amount for transfer
     * @param currencyCode current for transfer
     * @param surl         Success URL
     * @param furl         Failure URL
     */
    public void setTransactionParams(Long totalAmount, String currencyCode, String surl, String furl) {
        this.useModernGateway = true;
        this.totalAmount = totalAmount;
        this.currencyCode = currencyCode;
        this.surl = surl;
        this.furl = furl;
    }

    /**
     * Execute transaction, either using the Modern Gateway, when modernTransaction is set,
     * or using the Legacy Gateway when legacyTransaction is set.
     *
     * @return transaction ID from client
     */
    public String executeTransaction() {
        if (useModernGateway) {
            String modernTransaction = buildModernTransaction();
            if (modernTransaction != null) {
                return modernPaymentClient.execute(modernTransaction);
            }
        } else {
            String legacyTransaction = buildLegacyTransaction();
            String senderIP = userSession.getIP();
            if (legacyTransaction != null) {
                return legacyPaymentClient.execute(legacyTransaction, senderIP);
            }
        }
        return TRANSACTION_FAILURE;
    }

    private String buildLegacyTransaction() {
        if (currencyCode == null || totalAmount == null) {
            return null;
        }
        return String.join(";", currencyCode, totalAmount.toString());
    }

    private String buildModernTransaction() {
        if (currencyCode == null || totalAmount == null || surl == null || furl == null) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(new ModernTransaction()
                    .withExtOrderId(String.valueOf(UUID.randomUUID()))
                    .withTotalAmount(totalAmount)
                    .withCurrencyCode(currencyCode)
                    .withSurl(surl)
                    .withFurl(furl));
        } catch (JsonProcessingException e) {
            return null;
        }
    }
}
