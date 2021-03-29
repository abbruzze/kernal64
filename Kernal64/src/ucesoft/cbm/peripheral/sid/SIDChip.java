package ucesoft.cbm.peripheral.sid;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public interface SIDChip {
    void clock();
    int read(int offset);
    void write(int offset,int value);
    void setModel(int model);
    int output();

    void saveState(ObjectOutputStream out);
    void loadState(ObjectInputStream in);

    void reset();
    void updateBusValue(int value);
}
