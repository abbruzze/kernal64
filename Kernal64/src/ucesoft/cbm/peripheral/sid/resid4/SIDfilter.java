package ucesoft.cbm.peripheral.sid.resid4;

class SIDfilter {
    private boolean V3OFF;
    private boolean enabled;
    private int fc;
    private int res;
    private int filt;
    private boolean voice3off;
    private int hp_bp_lp;
    int vol;
    private int mixer_DC;
    private int Vhp;
    private int Vbp;
    private int Vlp;
    private int Vnf;
    private int DLthreshold;
    private int DLsteepness;
    private int DHthreshold;
    private int DHsteepness;
    private int DLlp;
    private int DLbp;
    private int DLhp;
    private int DHlp;
    private int DHbp;
    private int DHhp;
    private int w0;
    private int w0_ceil_1;
    private int _1024_div_Q;
    private final int[] f0_6581;
    private final int[] f0_8580;
    private int[] f0;
    static final int[][] f0_points_6581;
    static final int[][] f0_points_8580;
    private static final int[][] _1024_div_Q_table;
    private int sid_model;

    public void clock(final int voice1, final int voice2, final int voice3) {
        clock(voice1, voice2, voice3, 0);
    }

    public void clock(final int voice1, final int voice2, int voice3, final int ext_in) {
        if (voice3off) {
            voice3 = 0;
        }
        if (!enabled) {
            Vnf = voice1 + voice2 + voice3 + ext_in;
            final int vhp = 0;
            Vlp = vhp;
            Vbp = vhp;
            Vhp = vhp;
            return;
        }
        Vnf = 0;
        int Vi = 0;
        switch (filt) {
            default: {
                Vi = 0;
                Vnf = voice1 + voice2 + voice3 + ext_in;
                break;
            }
            case 1: {
                Vi = voice1;
                Vnf = voice2 + voice3 + ext_in;
                break;
            }
            case 2: {
                Vi = voice2;
                Vnf = voice1 + voice3 + ext_in;
                break;
            }
            case 3: {
                Vi = voice1 + voice2;
                Vnf = voice3 + ext_in;
                break;
            }
            case 4: {
                Vi = voice3;
                Vnf = voice1 + voice2 + ext_in;
                break;
            }
            case 5: {
                Vi = voice1 + voice3;
                Vnf = voice2 + ext_in;
                break;
            }
            case 6: {
                Vi = voice2 + voice3;
                Vnf = voice1 + ext_in;
                break;
            }
            case 7: {
                Vi = voice1 + voice2 + voice3;
                Vnf = ext_in;
                break;
            }
            case 8: {
                Vi = ext_in;
                Vnf = voice1 + voice2 + voice3;
                break;
            }
            case 9: {
                Vi = voice1 + ext_in;
                Vnf = voice2 + voice3;
                break;
            }
            case 10: {
                Vi = voice2 + ext_in;
                Vnf = voice1 + voice3;
                break;
            }
            case 11: {
                Vi = voice1 + voice2 + ext_in;
                Vnf = voice3;
                break;
            }
            case 12: {
                Vi = voice3 + ext_in;
                Vnf = voice1 + voice2;
                break;
            }
            case 13: {
                Vi = voice1 + voice3 + ext_in;
                Vnf = voice2;
                break;
            }
            case 14: {
                Vi = voice2 + voice3 + ext_in;
                Vnf = voice1;
                break;
            }
            case 15: {
                Vi = voice1 + voice2 + voice3 + ext_in;
                Vnf = 0;
                break;
            }
        }
        Vi >>= 7;
        Vnf >>= 7;
        int Vi_peak_bp = (Vlp * DHlp + Vbp * DHbp + Vhp * DHhp >> 8) + Vi;
        if (Vi_peak_bp < DHthreshold) {
            Vi_peak_bp = DHthreshold;
        }
        int Vi_peak_lp = (Vlp * DLlp + Vbp * DLbp + Vhp * DLhp >> 8) + Vi;
        if (Vi_peak_lp < DLthreshold) {
            Vi_peak_lp = DLthreshold;
        }
        int w0_eff_bp = w0 + w0 * (Vi_peak_bp - DHthreshold >> 4) / DHsteepness;
        int w0_eff_lp = w0 + w0 * (Vi_peak_lp - DLthreshold >> 4) / DLsteepness;
        if (w0_eff_bp > w0_ceil_1) {
            w0_eff_bp = w0_ceil_1;
        }
        if (w0_eff_lp > w0_ceil_1) {
            w0_eff_lp = w0_ceil_1;
        }
        Vhp = (Vbp * _1024_div_Q >> 10) - Vlp - Vi;
        Vlp -= w0_eff_lp * Vbp >> 20;
        Vbp -= w0_eff_bp * Vhp >> 20;
    }

    public int output() {
        if (!enabled) {
            return (Vnf + mixer_DC) * vol;
        }
        int Vf = 0;
        switch (hp_bp_lp) {
            default: {
                Vf = 0;
                break;
            }
            case 1: {
                Vf = Vlp;
                break;
            }
            case 2: {
                Vf = Vbp;
                break;
            }
            case 3: {
                Vf = Vlp + Vbp;
                break;
            }
            case 4: {
                Vf = Vhp;
                break;
            }
            case 5: {
                Vf = Vlp + Vhp;
                break;
            }
            case 6: {
                Vf = Vbp + Vhp;
                break;
            }
            case 7: {
                Vf = Vlp + Vbp + Vhp;
                break;
            }
        }
        return (Vnf + Vf + mixer_DC) * vol;
    }

    public SIDfilter() {
        V3OFF = false;
        f0_6581 = new int[2048];
        f0_8580 = new int[2048];
        fc = 0;
        res = 0;
        filt = 0;
        voice3off = false;
        hp_bp_lp = 0;
        vol = 0;
        Vhp = 0;
        Vbp = 0;
        Vlp = 0;
        Vnf = 0;
        enable_filter(true);
        DAC.interpolate(SIDfilter.f0_points_6581, 0, SIDfilter.f0_points_6581.length - 1, f0_6581, 1.0);
        DAC.interpolate(SIDfilter.f0_points_8580, 0, SIDfilter.f0_points_8580.length - 1, f0_8580, 1.0);
        set_chip_model(0);
        set_distortion_properties(999999, 999999, 0, 0, 0, 999999, 999999, 0, 0, 0);
    }

    private void enable_filter(final boolean enable) {
        enabled = enable;
    }

    public void set_chip_model(final int model) {
        sid_model = model;
        if (model == 0) {
            mixer_DC = -454;
            f0 = f0_6581;
        } else {
            mixer_DC = 0;
            f0 = f0_8580;
        }
        set_w0();
        set_Q();
    }

    private void set_distortion_properties(final int Lthreshold, int Lsteepness, final int Llp, final int Lbp, final int Lhp, final int Hthreshold, int Hsteepness, final int Hlp, final int Hbp, final int Hhp) {
        DLthreshold = Lthreshold;
        if (Lsteepness < 16) {
            Lsteepness = 16;
        }
        DLsteepness = Lsteepness >> 4;
        DLlp = Llp;
        DLbp = Lbp;
        DLhp = Lhp;
        DHthreshold = Hthreshold;
        if (Hsteepness < 16) {
            Hsteepness = 16;
        }
        DHsteepness = Hsteepness >> 4;
        DHlp = Hlp;
        DHbp = Hbp;
        DHhp = Hhp;
    }

    public void reset() {
        fc = 0;
        res = 0;
        filt = 0;
        voice3off = false;
        hp_bp_lp = 0;
        vol = 0;
        Vhp = 0;
        Vbp = 0;
        Vlp = 0;
        Vnf = 0;
        set_w0();
        set_Q();
    }

    public void writeFC_LO(final int fc_lo) {
        fc = ((fc & 0x7F8) | (fc_lo & 0x7));
        set_w0();
    }

    public void writeFC_HI(final int fc_hi) {
        fc = ((fc_hi << 3 & 0x7F8) | (fc & 0x7));
        set_w0();
    }

    public void writeRES_FILT(final int res_filt) {
        res = (res_filt >> 4 & 0xF);
        set_Q();
        filt = (res_filt & 0xF);
        voice3off = (V3OFF && (filt & 0x4) == 0x0);
    }

    public void writeMODE_VOL(final int mode_vol) {
        V3OFF = ((mode_vol & 0x80) == 0x80);
        voice3off = (V3OFF && (filt & 0x4) == 0x0);
        hp_bp_lp = (mode_vol >> 4 & 0x7);
        vol = (mode_vol & 0xF);
    }

    private void set_w0() {
        final double pi = 3.141592653589793;
        w0 = (int) (6.283185307179586 * f0[fc] * 1.048576);
        final int w0_max_1 = 118591;
        w0_ceil_1 = ((w0 <= w0_max_1) ? w0 : w0_max_1);
    }

    private void set_Q() {
        _1024_div_Q = SIDfilter._1024_div_Q_table[sid_model][res];
    }

    static {
        f0_points_6581 = new int[][]{{0, 220}, {0, 220}, {128, 230}, {256, 250}, {384, 300}, {512, 420}, {640, 780}, {768, 1600}, {832, 2300}, {896, 3200}, {960, 4300}, {992, 5000}, {1008, 5400}, {1016, 5700}, {1023, 6000}, {1023, 6000}, {1024, 4600}, {1024, 4600}, {1032, 4800}, {1056, 5300}, {1088, 6000}, {1120, 6600}, {1152, 7200}, {1280, 9500}, {1408, 12000}, {1536, 14500}, {1664, 16000}, {1792, 17100}, {1920, 17700}, {2047, 18000}, {2047, 18000}};
        f0_points_8580 = new int[][]{{0, 0}, {0, 0}, {128, 800}, {256, 1600}, {384, 2500}, {512, 3300}, {640, 4100}, {768, 4800}, {896, 5600}, {1024, 6500}, {1152, 7500}, {1280, 8400}, {1408, 9200}, {1536, 9800}, {1664, 10500}, {1792, 11000}, {1920, 11700}, {2047, 12500}, {2047, 12500}};
        _1024_div_Q_table = new int[2][16];
        for (int res = 0; res < 16; ++res) {
            SIDfilter._1024_div_Q_table[0][res] = (int) (1024.0 / (0.707 + 1.0 * res / 15.0));
            SIDfilter._1024_div_Q_table[1][res] = (int) (1024.0 * Math.pow(2.0, (4 - res) / 8.0) + 0.5);
        }
    }
}
