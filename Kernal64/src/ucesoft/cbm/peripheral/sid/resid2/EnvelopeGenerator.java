package ucesoft.cbm.peripheral.sid.resid2;

class EnvelopeGenerator {
    private static final int[] rate_counter_period;
    private static final int[] sustain_level;
    private int env3;
    int rate_period;
    int exponential_counter;
    int exponential_counter_period;
    int envelope_counter;
    int envelope_pipeline;
    private int attack;
    private int decay;
    private int sustain;
    private int release;
    private boolean gate;
    private int state;
    private int next_state;
    int rate_counter;
    private int exponential_pipeline;
    private int state_pipeline;
    private boolean reset_rate_counter;
    private boolean envON;
    int[] model_dac;
    private boolean update;

    EnvelopeGenerator() {
        reset();
    }

    private void update() {
        env3 = envelope_counter;
        if (state_pipeline > 0 && state_pipeline-- == 1) {
            state = next_state;
            if (state == 0) {
                rate_period = EnvelopeGenerator.rate_counter_period[attack];
                envON = true;
            } else {
                rate_period = EnvelopeGenerator.rate_counter_period[release];
            }
        }
        if (envelope_pipeline != 0 && --envelope_pipeline == 0 && envON) {
            if (state > 0) {
                --envelope_counter;
            } else if (++envelope_counter == 255) {
                state = 1;
                rate_period = EnvelopeGenerator.rate_counter_period[decay];
            }
            switch (envelope_counter &= 0xFF) {
                case 255: {
                    exponential_counter_period = 1;
                    break;
                }
                case 93: {
                    exponential_counter_period = 2;
                    break;
                }
                case 54: {
                    exponential_counter_period = 4;
                    break;
                }
                case 26: {
                    exponential_counter_period = 8;
                    break;
                }
                case 14: {
                    exponential_counter_period = 16;
                    break;
                }
                case 6: {
                    exponential_counter_period = 30;
                    break;
                }
                case 0: {
                    exponential_counter_period = 1;
                    envON = false;
                    break;
                }
            }
        }
        if (exponential_pipeline != 0 && --exponential_pipeline == 0) {
            exponential_counter = 0;
            if ((state == 1 && envelope_counter != EnvelopeGenerator.sustain_level[sustain]) || state == 2) {
                envelope_pipeline = 1;
            }
        } else if (reset_rate_counter) {
            reset_rate_counter = false;
            if (state == 0) {
                exponential_counter = 0;
                envelope_pipeline = 2;
            } else if (envON && ++exponential_counter == exponential_counter_period) {
                exponential_pipeline = ((exponential_counter_period == 1) ? 1 : 2);
            }
        }
        update = ((state_pipeline | envelope_pipeline | exponential_pipeline) > 0 || env3 != envelope_counter);
    }

    void clock() {
        if (update) {
            update();
        }
        if (rate_counter == rate_period) {
            final boolean b = true;
            update = b;
            reset_rate_counter = b;
            rate_counter = 0;
        } else if (++rate_counter > 32767) {
            rate_counter = 1;
        }
    }

    int output() {
        return model_dac[envelope_counter];
    }

    void writeControlReg(final int control) {
        if (gate ^ (control & 0x1) == 0x1) {
            final boolean gate = this.gate ^ true;
            this.gate = gate;
            next_state = (gate ? 0 : 2);
            if (next_state == 0) {
                state = 1;
                rate_period = EnvelopeGenerator.rate_counter_period[decay];
                state_pipeline = 2;
                if (reset_rate_counter || exponential_pipeline == 2) {
                    envelope_pipeline = ((exponential_counter_period == 1 || exponential_pipeline == 2) ? 2 : 4);
                } else if (exponential_pipeline == 1) {
                    state_pipeline = 3;
                }
            } else if (envON) {
                state_pipeline = ((envelope_pipeline > 0) ? 3 : 2);
            }
            update |= (state_pipeline > 0);
        }
    }

    void writeAttackDecay(final int attack_decay) {
        attack = (attack_decay >> 4 & 0xF);
        decay = (attack_decay & 0xF);
        if (state == 0) {
            rate_period = rate_counter_period[attack];
        } else if (state == 1) {
            rate_period = rate_counter_period[decay];
        }
    }

    void writeSustainRelease(final int sustain_release) {
        sustain = (sustain_release >> 4 & 0xF);
        release = (sustain_release & 0xF);
        if (state == 2) {
            rate_period = rate_counter_period[release];
        }
    }

    int readENV() {
        return env3;
    }

    void reset() {
        final int n = 170;
        envelope_counter = n;
        env3 = n;
        gate = false;
        exponential_counter = 0;
        exponential_counter_period = 1;
        state_pipeline = 0;
        exponential_pipeline = 0;
        envelope_pipeline = 0;
        release = 0;
        sustain = 0;
        decay = 0;
        attack = 0;
        rate_counter = 0;
        reset_rate_counter = false;
        state = 2;
        rate_period = rate_counter_period[release];
        envON = true;
        update = true;
    }

    static {
        rate_counter_period = new int[]{8, 31, 62, 94, 148, 219, 266, 312, 391, 976, 1953, 3125, 3906, 11719, 19531, 31250};
        sustain_level = new int[]{0, 17, 34, 51, 68, 85, 102, 119, 136, 153, 170, 187, 204, 221, 238, 255};
    }
}
