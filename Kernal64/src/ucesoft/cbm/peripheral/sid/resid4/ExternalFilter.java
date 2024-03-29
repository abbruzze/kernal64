package ucesoft.cbm.peripheral.sid.resid4;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

class ExternalFilter {
    private int Vlp;
    private int Vhp;
    private int Vo;
    private int w0lp;
    private int w0hp;
    final private int w0lp_1_s7;
    final private int w0hp_1_s17;

    public void clock(final int Vi) {
        final int dVlp = (w0lp >> 8) * (Vi - Vlp) >> 12;
        final int dVhp = w0hp * (Vlp - Vhp) >> 20;
        Vo = Vlp - Vhp;
        Vlp += dVlp;
        Vhp += dVhp;
    }

    public int output() {
        return Vo;
    }

    public ExternalFilter() {
        w0lp_1_s7 = 13;
        w0hp_1_s17 = 13;
        reset();
        set_sampling_parameter(15915.6);
    }

    private void set_sampling_parameter(final double pass_freq) {
        w0hp = 105;
        w0lp = (int) (pass_freq * 6.588397316661141);
        if (w0lp > 104858) {
            w0lp = 104858;
        }
    }

    public void reset() {
        Vlp = 0;
        Vhp = 0;
        Vo = 0;
    }

    void saveState(ObjectOutputStream out) throws IOException {
        out.writeInt(Vlp);
        out.writeInt(Vhp);
        out.writeInt(Vlp);
    }

    void loadState(ObjectInputStream in) throws IOException {
        Vlp = in.readInt();
        Vhp = in.readInt();
        Vlp = in.readInt();
    }
}
