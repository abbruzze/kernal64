package ucesoft.cbm.peripheral.sid.resid4;

class Voice {
    final WaveformGenerator wave;
    final EnvelopeGenerator envelope;
    private int wave_zero;
    private int voice_DC;
    boolean ON;

    Voice() {
        wave = new WaveformGenerator();
        envelope = new EnvelopeGenerator();
        ON = true;
    }

    void setSYNCsource(final Voice src) {
        wave.set_sync_source(src.wave);
    }

    void switch_model(final SIDModel model) {
        wave.set_waveforms(model);
        envelope.model_dac = model.model_dacE;
        wave_zero = model.wave_zero;
        voice_DC = model.voice_DC;
    }

    int generate() {
        return ON ? (envelope.output() * (wave.output() - wave_zero) + voice_DC) : voice_DC;
    }

    int generateRAW() {
        return ON ? (envelope.output() * wave.output()) : 0;
    }

    void reset() {
        envelope.reset();
        wave.reset();
    }
}
