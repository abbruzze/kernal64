package ucesoft.cbm.peripheral.sid.resid4;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public class WaveformGenerator {
    private final int FLOATING_OUTPUT_TTL_6581 = 200000;
    private final int FLOATING_OUTPUT_TTL_8580 = 5000000;
    private final int SHIFT_REGISTER_RESET_6581 = 32768;
    private final int SHIFT_REGISTER_RESET_8580 = 9764864;

    private int sid_model;
    private int accumulator;
    private int tri_saw_pipeline;
    private int freq;
    private int pw;
    private boolean msb_rising;
    private int waveform;
    private boolean test;
    private int ring_mod;
    private boolean sync;
    private int ring_msb_mask;
    private int no_noise;
    private int no_pulse;
    private int pulse_output;
    private int shift_register;
    private int shift_register_reset;
    private int shift_pipeline;
    private int waveform_output;
    private int osc3;
    private int floating_output_ttl;
    private int noise_output;
    private int no_noise_or_noise_output;
    private int[] wave;
    private int[][] waveforms;
    private int[] model_dac;
    private WaveformGenerator sync_source;
    private WaveformGenerator sync_dest;

    public WaveformGenerator() {
        accumulator = 5592405;
        tri_saw_pipeline = 1365;
    }

    void set_sync_source(final WaveformGenerator source) {
        sync_source = source;
        source.sync_dest = this;
    }

    void clock() {
        if (test) {
            if (shift_register_reset != 0 && --shift_register_reset == 0) {
                reset_shift_register();
            }
            pulse_output = 4095;
        } else {
            final int accumulator_next = accumulator + freq & 0xFFFFFF;
            final int accumulator_bits_set = ~accumulator & accumulator_next;
            accumulator = accumulator_next;
            msb_rising = ((accumulator_bits_set & 0x800000) != 0x0);
            if ((accumulator_bits_set & 0x80000) != 0x0) {
                shift_pipeline = 2;
            } else if (shift_pipeline != 0 && --shift_pipeline == 0) {
                clock_shift_register();
            }
        }
    }

    void reset_shift_register() {
        shift_register = 8388607;
        shift_register_reset = 0;
        set_noise_output();
    }

    void clock_shift_register() {
        final int bit0 = (shift_register >> 22 ^ shift_register >> 17) & 0x1;
        shift_register = ((shift_register << 1 | bit0) & 0x7FFFFF);
        set_noise_output();
    }

    void writeFREQ_LO(final int freq_lo) {
        freq = ((freq & 0xFF00) | (freq_lo & 0xFF));
    }

    void writeFREQ_HI(final int freq_hi) {
        freq = ((freq_hi << 8 & 0xFF00) | (freq & 0xFF));
    }

    void writePW_LO(final int pw_lo) {
        pw = ((pw & 0xF00) | (pw_lo & 0xFF));
        pulse_output = ((accumulator >> 12 >= pw) ? 4095 : 0);
    }

    void writePW_HI(final int pw_hi) {
        pw = ((pw_hi << 8 & 0xF00) | (pw & 0xFF));
        pulse_output = ((accumulator >> 12 >= pw) ? 4095 : 0);
    }

    void writeCONTROL_REG(final int control) {
        final int waveform_prev = waveform;
        final boolean test_prev = test;
        waveform = (control >> 4 & 0xF);
        test = ((control & 0x8) != 0x0);
        ring_mod = (control & 0x4);
        sync = ((control & 0x2) != 0x0);
        wave = waveforms[waveform & 0x7];
        ring_msb_mask = (~control >> 5 & control >> 2 & 0x1) << 23;
        no_noise = (((waveform & 0x8) != 0x0) ? 0 : 4095);
        no_noise_or_noise_output = (no_noise | noise_output);
        no_pulse = (((waveform & 0x4) != 0x0) ? 0 : 4095);
        if (test && !test_prev) {
            accumulator = 0;
            shift_pipeline = 0;
            shift_register_reset = ((sid_model == 0) ? SHIFT_REGISTER_RESET_6581 : SHIFT_REGISTER_RESET_8580);
            pulse_output = 4095;
        } else if (test_prev && !test) {
            if (do_pre_writeback(waveform_prev, waveform, sid_model == 0)) {
                write_shift_register();
            }
            final int bit0 = ~shift_register >> 17 & 0x1;
            shift_register = ((shift_register << 1 | bit0) & 0x7FFFFF);
            set_noise_output();
        }
        if (waveform != 0) {
            set_waveform_output();
        } else if (waveform_prev != 0) {
            floating_output_ttl = ((sid_model == 0) ? FLOATING_OUTPUT_TTL_6581 : FLOATING_OUTPUT_TTL_8580);
        }
    }

    boolean do_pre_writeback(final int waveform_prev, final int waveform, final boolean is6581) {
        return waveform_prev > 8 && waveform != 8 && waveform_prev != 12 && (!is6581 || (((waveform_prev & 0x3) != 0x1 || (waveform & 0x3) != 0x2) && ((waveform_prev & 0x3) != 0x2 || (waveform & 0x3) != 0x1)));
    }

    void write_shift_register() {
        shift_register &= (0xFFEBB5DA | (waveform_output & 0x800) << 9 | (waveform_output & 0x400) << 8 | (waveform_output & 0x200) << 5 | (waveform_output & 0x100) << 3 | (waveform_output & 0x80) << 2 | (waveform_output & 0x40) >> 1 | (waveform_output & 0x20) >> 3 | (waveform_output & 0x10) >> 4);
        noise_output &= waveform_output;
        no_noise_or_noise_output = (no_noise | noise_output);
    }

    static int noise_pulse6581(final int noise) {
        return (noise < 3840) ? 0 : (noise & noise << 1 & noise << 2);
    }

    static int noise_pulse8580(final int noise) {
        return (noise < 4032) ? (noise & noise << 1) : 4032;
    }

    private void set_noise_output() {
        noise_output = ((shift_register & 0x100000) >> 9 | (shift_register & 0x40000) >> 8 | (shift_register & 0x4000) >> 5 | (shift_register & 0x800) >> 3 | (shift_register & 0x200) >> 2 | (shift_register & 0x20) << 1 | (shift_register & 0x4) << 3 | (shift_register & 0x1) << 4);
        no_noise_or_noise_output = (no_noise | noise_output);
    }

    int output() {
        return model_dac[waveform_output];
    }

    void synchronize() {
        if (msb_rising && sync_dest.sync && (!sync || !sync_source.msb_rising)) {
            sync_dest.accumulator = 0;
        }
    }

    void set_waveform_output() {
        if (waveform != 0) {
            final int ix = (accumulator ^ (~sync_source.accumulator & ring_msb_mask)) >> 12;
            waveform_output = (wave[ix] & (no_pulse | pulse_output) & no_noise_or_noise_output);
            if ((waveform & 0xC) == 0xC) {
                waveform_output = ((sid_model == 0) ? noise_pulse6581(waveform_output) : noise_pulse8580(waveform_output));
            }
            if ((waveform & 0x3) != 0x0 && sid_model == 1) {
                osc3 = (tri_saw_pipeline & (no_pulse | pulse_output) & no_noise_or_noise_output);
                tri_saw_pipeline = wave[ix];
            } else {
                osc3 = waveform_output;
            }
            if ((waveform & 0x2) != 0x0 && (waveform & 0xD) != 0x0 && sid_model == 0) {
                accumulator &= (waveform_output << 12 | 0x7FFFFF);
            }
            if (waveform > 8 && !test && shift_pipeline != 1) {
                write_shift_register();
            }
        } else if (floating_output_ttl != 0 && --floating_output_ttl == 0) {
            waveform_output = 0;
        }
        pulse_output = ((accumulator >> 12 >= pw) ? 4095 : 0);
    }

    int readOSC() {
        return osc3 >> 4;
    }

    public void reset() {
        freq = 0;
        pw = 0;
        msb_rising = false;
        waveform = 0;
        test = false;
        ring_mod = 0;
        sync = false;
        wave = waveforms[0];
        ring_msb_mask = 0;
        no_noise = 4095;
        no_pulse = 4095;
        pulse_output = 4095;
        shift_register = 8388606;
        shift_register_reset = 0;
        set_noise_output();
        shift_pipeline = 0;
        waveform_output = 0;
        osc3 = 0;
        floating_output_ttl = 0;
    }

    void set_waveforms(final SIDModel model) {
        sid_model = model.id;
        waveforms = model.waveforms;
        wave = waveforms[waveform & 0x7];
        model_dac = model.model_dacW;
    }

    void saveState(ObjectOutputStream out) throws IOException {
        out.writeInt(accumulator);
        out.writeInt(shift_register);
        out.writeInt(shift_pipeline);
        out.writeInt(shift_register_reset);
        out.writeInt(floating_output_ttl);
        out.writeInt(tri_saw_pipeline);
        out.writeInt(freq);
        out.writeInt(pw);
        out.writeBoolean(msb_rising);
        out.writeInt(waveform);
        out.writeBoolean(test);
        out.writeInt(ring_mod);
        out.writeBoolean(sync);
        out.writeInt(ring_msb_mask);
        out.writeInt(no_noise);
        out.writeInt(no_pulse);
        out.writeInt(pulse_output);
        out.writeInt(waveform_output);
        out.writeInt(osc3);
        out.writeInt(noise_output);
        out.writeInt(no_noise_or_noise_output);
    }

    void loadState(ObjectInputStream in) throws IOException {
        accumulator = in.readInt();
        shift_register = in.readInt();
        shift_pipeline = in.readInt();
        shift_register_reset = in.readInt();
        floating_output_ttl = in.readInt();
        tri_saw_pipeline = in.readInt();
        freq = in.readInt();
        pw = in.readInt();
        msb_rising = in.readBoolean();
        waveform = in.readInt();
        test = in.readBoolean();
        ring_mod = in.readInt();
        sync = in.readBoolean();
        ring_msb_mask = in.readInt();
        no_noise = in.readInt();
        no_pulse = in.readInt();
        pulse_output = in.readInt();
        waveform_output = in.readInt();
        osc3 = in.readInt();
        noise_output = in.readInt();
        no_noise_or_noise_output = in.readInt();
    }
}
