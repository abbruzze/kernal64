package ucesoft.cbm.peripheral.sid.resid4;

import java.io.IOException;
import java.io.InputStream;

class SIDModel {
    final int id;
    final int wave_zero;
    final int voice_DC;
    public final int[][] waveforms;
    final int[] model_dacW;
    final int[] model_dacE;

    private static int[] getResourceAsInt(final String filename) {
        InputStream in = SIDModel.class.getClassLoader().getResourceAsStream("resources/" + filename);
        final byte[] output;
        try {
            output = in.readAllBytes();
        } catch (IOException e) {
            throw new RuntimeException("Can't find resource " + filename);
        }
        final int[] resource = new int[output.length];
        for (int i = 0; i < resource.length; ++i) {
            resource[i] = output[i] & 0xFF;
        }
        return resource;
    }

    SIDModel(final String type, final double r, final int wave_zero, final int voice_DC, final boolean term, final int id) {
        waveforms = new int[8][4096];
        this.id = id;
        this.wave_zero = wave_zero;
        this.voice_DC = voice_DC;
        model_dacW = DAC.build_dac_table(12, r, term);
        model_dacE = DAC.build_dac_table(8, r, term);
        waveforms[3] = getResourceAsInt("sid/wave" + type + "__ST.dat");
        waveforms[5] = getResourceAsInt("sid/wave" + type + "_P_T.dat");
        waveforms[6] = getResourceAsInt("sid/wave" + type + "_PS_.dat");
        waveforms[7] = getResourceAsInt("sid/wave" + type + "_PST.dat");
        int accumulator = 0;
        for (int i = 0; i < 4096; ++i) {
            waveforms[0][i] = 4095;
            waveforms[1][i] = ((((accumulator & 0x800000) == 0x800000) ? (~accumulator) : accumulator) >> 11 & 0xFFE);
            waveforms[2][i] = accumulator >> 12;
            waveforms[3][i] <<= 4;
            waveforms[4][i] = 4095;
            waveforms[5][i] <<= 4;
            waveforms[6][i] <<= 4;
            waveforms[7][i] <<= 4;
            accumulator += 4096;
        }
    }
}
