package ucesoft.cbm.peripheral.sid.resid4;

import ucesoft.cbm.Tickable;
import ucesoft.cbm.peripheral.sid.SIDChip;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public class SID implements SIDChip {
    private SIDModel current_model;
    public boolean enabled;
    private static final SIDModel MOS_6581;
    private static final SIDModel MOS_8580;
    private final Voice[] voices;
    public final Tickable clock;
    private final int[] register;
    private final SIDfilter filter;
    private final ExternalFilter extfilt;
    private int bus_value;
    private long bus_clock;
    public int type;

    public SID(final Tickable clock) {
        voices = new Voice[3];
        register = new int[32];
        filter = new SIDfilter();
        extfilt = new ExternalFilter();
        type = 0;
        voices[0] = new Voice();
        voices[1] = new Voice();
        voices[2] = new Voice();
        voices[0].setSYNCsource(voices[2]);
        voices[1].setSYNCsource(voices[0]);
        voices[2].setSYNCsource(voices[1]);
        this.clock = clock;
        setModel(type);
    }

    public void clock() {
        voices[0].envelope.clock();
        voices[1].envelope.clock();
        voices[2].envelope.clock();
        voices[0].wave.clock();
        voices[1].wave.clock();
        voices[2].wave.clock();
        voices[0].wave.synchronize();
        voices[1].wave.synchronize();
        voices[2].wave.synchronize();
        voices[0].wave.set_waveform_output();
        voices[1].wave.set_waveform_output();
        voices[2].wave.set_waveform_output();
        filter.clock(voices[0].generate(), voices[1].generate(), voices[2].generate());
        extfilt.clock(filter.output());
    }

    @Override
    final public int read(int offset) {
        switch (offset & 0x1F) {
            case 25:
            case 26: { // must be managed externally
                bus_value = 0;
                bus_clock = clock.currentCycles();
                break;
            }
            case 27: {
                bus_value = voices[2].wave.readOSC();
                bus_clock = clock.currentCycles();
                break;
            }
            case 28: {
                bus_value = voices[2].envelope.readENV();
                bus_clock = clock.currentCycles();
                break;
            }
            default: {
                updateBus();
                break;
            }
        }
        return bus_value;
    }
    @Override
    final public void write(int offset, int value) {
        bus_clock = clock.currentCycles();
        bus_value = (register[offset &= 0x1F] = value);
        switch (offset) {
            case 0: {
                voices[0].wave.writeFREQ_LO(value);
                break;
            }
            case 1: {
                voices[0].wave.writeFREQ_HI(value);
                break;
            }
            case 2: {
                voices[0].wave.writePW_LO(value);
                break;
            }
            case 3: {
                voices[0].wave.writePW_HI(value);
                break;
            }
            case 4: {
                voices[0].envelope.writeCONTROL_REG(value);
                voices[0].wave.writeCONTROL_REG(value);
                break;
            }
            case 5: {
                voices[0].envelope.writeATTACK_DECAY(value);
                break;
            }
            case 6: {
                voices[0].envelope.writeSUSTAIN_RELEASE(value);
                break;
            }
            case 7: {
                voices[1].wave.writeFREQ_LO(value);
                break;
            }
            case 8: {
                voices[1].wave.writeFREQ_HI(value);
                break;
            }
            case 9: {
                voices[1].wave.writePW_LO(value);
                break;
            }
            case 10: {
                voices[1].wave.writePW_HI(value);
                break;
            }
            case 11: {
                voices[1].envelope.writeCONTROL_REG(value);
                voices[1].wave.writeCONTROL_REG(value);
                break;
            }
            case 12: {
                voices[1].envelope.writeATTACK_DECAY(value);
                break;
            }
            case 13: {
                voices[1].envelope.writeSUSTAIN_RELEASE(value);
                break;
            }
            case 14: {
                voices[2].wave.writeFREQ_LO(value);
                break;
            }
            case 15: {
                voices[2].wave.writeFREQ_HI(value);
                break;
            }
            case 16: {
                voices[2].wave.writePW_LO(value);
                break;
            }
            case 17: {
                voices[2].wave.writePW_HI(value);
                break;
            }
            case 18: {
                voices[2].envelope.writeCONTROL_REG(value);
                voices[2].wave.writeCONTROL_REG(value);
                break;
            }
            case 19: {
                voices[2].envelope.writeATTACK_DECAY(value);
                break;
            }
            case 20: {
                voices[2].envelope.writeSUSTAIN_RELEASE(value);
                break;
            }
            case 21: {
                filter.writeFC_LO(value);
                break;
            }
            case 22: {
                filter.writeFC_HI(value);
                break;
            }
            case 23: {
                filter.writeRES_FILT(value);
                break;
            }
            case 24: {
                filter.writeMODE_VOL(value);
                break;
            }
        }
    }

    public void updateBus() {
        if (clock.currentCycles() - bus_clock > ((current_model == SID.MOS_8580) ? 663552 : 7424)) {
            bus_value = 0;
        }
    }

    @Override public void updateBusValue(int value) {
        bus_value = value;
    }

    public int output() {
        final int sample = extfilt.output() / 11;
        if (sample >= 32768) {
            return 32767;
        }
        return Math.max(sample, -32768);
    }

    public void setModel(final int type) {
        this.type = type;
        if (type == 0) {
            current_model = SID.MOS_6581;
        } else if (type == 1) {
            current_model = SID.MOS_8580;
        }
        voices[0].switch_model(current_model);
        voices[1].switch_model(current_model);
        voices[2].switch_model(current_model);
        filter.set_chip_model(type);
    }

    public void reset() {
        voices[0].reset();
        voices[1].reset();
        voices[2].reset();
        filter.reset();
        extfilt.reset();
        bus_value = 0;
    }

    @Override
    public void saveState(ObjectOutputStream out) {
        try {
            updateBus();
            out.writeObject(register);
            out.writeInt(bus_value);
            out.writeLong(bus_clock);
            for (final Voice voice : voices) {
                out.writeInt(voice.wave.accumulator);
                out.writeInt(voice.wave.shift_register);
                out.writeInt(voice.wave.shift_pipeline);
                out.writeInt(voice.wave.shift_register_reset);
                out.writeInt(voice.wave.floating_output_ttl);
                out.writeInt(voice.wave.tri_saw_pipeline);
                out.writeInt(voice.wave.freq);
                out.writeInt(voice.wave.pw);
                out.writeBoolean(voice.wave.msb_rising);
                out.writeInt(voice.wave.waveform);
                out.writeBoolean(voice.wave.test);
                out.writeInt(voice.wave.ring_mod);
                out.writeBoolean(voice.wave.sync);
                out.writeInt(voice.wave.ring_msb_mask);
                out.writeInt(voice.wave.no_noise);
                out.writeInt(voice.wave.no_pulse);
                out.writeInt(voice.wave.pulse_output);
                out.writeInt(voice.wave.waveform_output);
                out.writeInt(voice.wave.osc3);
                out.writeInt(voice.wave.noise_output);
                out.writeInt(voice.wave.no_noise_or_noise_output);

                out.writeInt(voice.envelope.env3);
                out.writeInt(voice.envelope.attack);
                out.writeInt(voice.envelope.decay);
                out.writeInt(voice.envelope.sustain);
                out.writeInt(voice.envelope.release);
                out.writeBoolean(voice.envelope.gate);
                out.writeInt(voice.envelope.state);
                out.writeInt(voice.envelope.next_state);
                out.writeInt(voice.envelope.exponential_pipeline);
                out.writeInt(voice.envelope.state_pipeline);
                out.writeBoolean(voice.envelope.reset_rate_counter);
                out.writeBoolean(voice.envelope.envON);
                out.writeBoolean(voice.envelope.update);
                out.writeInt(voice.envelope.rate_counter);
                out.writeInt(voice.envelope.exponential_counter);
                out.writeInt(voice.envelope.envelope_counter);
                out.writeInt(voice.envelope.rate_period);
                out.writeInt(voice.envelope.exponential_counter_period);
                out.writeInt(voice.envelope.envelope_pipeline);

            }
        }
        catch (Throwable e) {
            throw new RuntimeException("Can't save SID state",e);
        }
    }
    @Override
    public void loadState(ObjectInputStream in) {
        try {
            int[] regs = (int[])in.readObject();
            for(int i = 0;i < register.length;i++) {
                write(i,regs[i]);
            }
            bus_value = in.readInt();
            bus_clock = in.readLong();
            for (final Voice voice : voices) {
                voice.wave.accumulator = in.readInt();
                voice.wave.shift_register = in.readInt();
                voice.wave.shift_pipeline = in.readInt();
                voice.wave.shift_register_reset = in.readInt();
                voice.wave.floating_output_ttl = in.readInt();
                voice.wave.tri_saw_pipeline = in.readInt();
                voice.wave.freq = in.readInt();
                voice.wave.pw = in.readInt();
                voice.wave.msb_rising = in.readBoolean();
                voice.wave.waveform = in.readInt();
                voice.wave.test = in.readBoolean();
                voice.wave.ring_mod = in.readInt();
                voice.wave.sync = in.readBoolean();
                voice.wave.ring_msb_mask = in.readInt();
                voice.wave.no_noise = in.readInt();
                voice.wave.no_pulse = in.readInt();
                voice.wave.pulse_output = in.readInt();
                voice.wave.waveform_output = in.readInt();
                voice.wave.osc3 = in.readInt();
                voice.wave.noise_output = in.readInt();
                voice.wave.no_noise_or_noise_output = in.readInt();

                voice.envelope.env3 = in.readInt();
                voice.envelope.attack = in.readInt();
                voice.envelope.decay = in.readInt();
                voice.envelope.sustain = in.readInt();
                voice.envelope.release = in.readInt();
                voice.envelope.gate = in.readBoolean();
                voice.envelope.state = in.readInt();
                voice.envelope.next_state = in.readInt();
                voice.envelope.exponential_pipeline = in.readInt();
                voice.envelope.state_pipeline = in.readInt();
                voice.envelope.reset_rate_counter = in.readBoolean();
                voice.envelope.envON = in.readBoolean();
                voice.envelope.update = in.readBoolean();
                voice.envelope.rate_counter = in.readInt();
                voice.envelope.exponential_counter = in.readInt();
                voice.envelope.envelope_counter = in.readInt();
                voice.envelope.rate_period = in.readInt();
                voice.envelope.exponential_counter_period = in.readInt();
                voice.envelope.envelope_pipeline = in.readInt();

            }
        }
        catch (Throwable e) {
            throw new RuntimeException("Can't load SID state",e);
        }
    }

    static {
        MOS_6581 = new SIDModel("6581", 2.2, 896, 522240, false, 0);
        MOS_8580 = new SIDModel("8580", 2.0, 2528, 0, true, 1);
    }
}
