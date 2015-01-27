package ucesoft.c64.peripheral.sid.resid2;

/**
 * Filter based on Dag Lem's 6581 filter from reSID 1.0 prerelease. See original
 * source for discussion about theory of operation.
 * 
 * Java port by Antti S. Lankila
 * 
 * @author Ken Handel
 * @author Dag Lem
 * @author Antti Lankila
 */
public final class Filter6581 extends Filter {
	/** Filter highpass state. */
	private int Vhp;
	
	/** Filter bandpass state. */
	private int Vbp;
	
	/** Filter lowpass state. */
	private int Vlp;

	/** Current volume amplifier setting. */
	private char[] currentGain;
	
	/** Current filter/voice mixer setting. */
	private char[] currentMixer;
	
	/** Filter input summer setting. */
	private char[] currentSummer;
	
	/** Filter resonance value. */
	private char[] currentResonance;

	/** VCR + associated capacitor connected to highpass output. */
	private final Integrator hpIntegrator;
	
	/** VCR + associated capacitor connected to lowpass output. */
	private final Integrator bpIntegrator;

	/** Filter external input. */
	private int ve;

	private final int voiceScaleS14, voiceDC, vo_T16;
	
	private final char[] f0_dac;
	
	private final char[][] mixer, summer, gain;
	
	protected Filter6581() {
		super();
		
		voiceScaleS14 = FilterModelConfig.getVoiceScaleS14();
		voiceDC = FilterModelConfig.getVoiceDC();
		vo_T16 = FilterModelConfig.getVO_T16();
		f0_dac = FilterModelConfig.getDAC(FilterModelConfig.getDacZero(0));
		mixer = FilterModelConfig.getMixer();
		summer = FilterModelConfig.getSummer();
		gain = FilterModelConfig.getGain();
		hpIntegrator = FilterModelConfig.buildIntegrator();
		bpIntegrator = FilterModelConfig.buildIntegrator();

		input(0);
	}
	
	/**
	 * Set filter curve type based on single parameter.
	 * 
	 * @param curvePosition 0 .. 1, where 0 sets center frequency high ("light") and 1 sets it low ("dark")
	 */
	public void setFilterCurve(double curvePosition) {
		System.arraycopy(FilterModelConfig.getDAC(FilterModelConfig.getDacZero(curvePosition)), 0, f0_dac, 0, 2048);
		updatedCenterFrequency();
	}
	
	@Override
	protected int clock(final int voice1, final int voice2, final int voice3) {
		int v1 = (voice1 * voiceScaleS14 >> 18) + voiceDC;
		int v2 = (voice2 * voiceScaleS14 >> 18) + voiceDC;
		int v3 = (voice3 * voiceScaleS14 >> 18) + voiceDC;

		int Vi = 0;
		int Vo = 0;

		if (filt1) {
			Vi += v1;
		} else {
			Vo += v1;
		}
		if (filt2) {
			Vi += v2;
		} else {
			Vo += v2;
		}
		// NB! Voice 3 is not silenced by voice3off if it is routed
		// through the filter.
		if (filt3) {
			Vi += v3;
		} else if (!voice3off) {
			Vo += v3;
		}
		if (filtE) {
			Vi += ve;
		} else {
			Vo += ve;
		}

		int oldVhp = Vhp;
		Vhp = currentSummer[currentResonance[Vbp] + Vlp + Vi];
		Vlp = bpIntegrator.solve(Vbp + vo_T16) - vo_T16;
		Vbp = hpIntegrator.solve(oldVhp + vo_T16) - vo_T16;

		if (lp) {
			Vo += Vlp;
		}
		if (bp) {
			Vo += Vbp;
		}
		if (hp) {
			Vo += Vhp;
		}
		return currentGain[currentMixer[Vo]] - (1 << 15);
	}

	@Override
	protected void input(int sample) {
		ve = (sample * voiceScaleS14 * 3 >> 10) + mixer[0][0];
	}

	/**
	 * Switch to new distortion curve.
	 */
	@Override
	protected void updatedCenterFrequency() {
		int Vw = f0_dac[fc];
		hpIntegrator.setVw(Vw);
		bpIntegrator.setVw(Vw);
	}

	/**
	 * Resonance tuned by ear, based on a few observations:
	 * 
	 * - there's a small notch even in allpass mode - size of resonance hump is
	 * about 8 dB
	 */
	@Override
	protected void updatedResonance() {
		currentResonance = gain[~res & 0xf];
	}

	@Override
	protected void updatedMixing() {
		currentGain = gain[vol];

		int ni = 0;
		int no = 0;

		if (filt1) {
			ni++;
		} else {
			no++;
		}
		if (filt2) {
			ni++;
		} else {
			no++;
		}
		if (filt3) {
			ni++;
		} else if (!voice3off) {
			no++;
		}
		if (filtE) {
			ni++;
		} else {
			no++;
		}
		currentSummer = summer[ni];

		if (lp) {
			no++;
		}
		if (bp) {
			no++;
		}
		if (hp) {
			no++;
		}
		currentMixer = mixer[no];
	}
}
