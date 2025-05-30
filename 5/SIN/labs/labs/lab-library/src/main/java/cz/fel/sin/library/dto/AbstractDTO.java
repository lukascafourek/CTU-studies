package cz.fel.sin.library.dto;

public abstract class AbstractDTO implements Cloneable {

    public AbstractDTO() {
    }

    protected AbstractDTO clone() throws CloneNotSupportedException {
        return (AbstractDTO) super.clone();
    }
}
