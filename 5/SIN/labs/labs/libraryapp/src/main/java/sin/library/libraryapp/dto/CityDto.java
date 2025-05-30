package sin.library.libraryapp.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class CityDto {

    Integer id;
    String name;
    List<Integer> addressesIds;

    public List<Integer> getAddressesIds() {
        if (addressesIds != null) {
            return new ArrayList<>(addressesIds);
        }
        return null;
    }

    public void setAddressesIds(List<Integer> addressesIds) {
        if (addressesIds != null) {
            this.addressesIds = new ArrayList<>(addressesIds);
        }
    }
}
