package cz.cvut.fel.omo.homeworks.common.model;

public class ModernTransaction {
    private Long totalAmount;
    private String currencyCode;
    private String extOrderId;
    private String surl;
    private String furl;

    public ModernTransaction withTotalAmount(Long totalAmount) {
        this.totalAmount = totalAmount;
        return this;
    }

    public ModernTransaction withCurrencyCode(String currencyCode) {
        this.currencyCode = currencyCode;
        return this;
    }

    public ModernTransaction withExtOrderId(String extOrderId) {
        this.extOrderId = extOrderId;
        return this;
    }

    public ModernTransaction withSurl(String surl) {
        this.surl = surl;
        return this;
    }

    public ModernTransaction withFurl(String furl) {
        this.furl = furl;
        return this;
    }

    public Long getTotalAmount() {
        return totalAmount;
    }

    public String getCurrencyCode() {
        return currencyCode;
    }

    public String getExtOrderId() {
        return extOrderId;
    }

    public String getSurl() {
        return surl;
    }

    public String getFurl() {
        return furl;
    }

    @Override
    public String toString() {
        return "ModernTransaction{" +
                "totalAmount=" + totalAmount +
                ", currencyCode='" + currencyCode + '\'' +
                ", extOrderId='" + extOrderId + '\'' +
                ", surl='" + surl + '\'' +
                ", furl='" + furl + '\'' +
                '}';
    }
}
