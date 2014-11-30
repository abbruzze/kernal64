package ucesoft.c64.peripheral.sid.resid2;

public final class FilterModelConfig {
	private static final double[][] opamp_voltage = {
			{ 0.75, 10.02 }, // Approximate start of actual range
			{ 2.50, 10.13 },
			{ 2.75, 10.12 },
			{ 2.90, 10.04 },
			{ 3.00, 9.92 },
			{ 3.10, 9.74 },
			{ 3.25, 9.40 },
			{ 3.50, 8.68 },
			{ 4.00, 6.90 },
			{ 4.25, 5.88 },
			{ 4.53, 4.53 }, // Working point (vi = vo)
			{ 4.75, 3.20 },
			{ 4.90, 2.30 }, // Change of curvature
			{ 4.95, 2.05 },
			{ 5.00, 1.90 },
			{ 5.10, 1.71 },
			{ 5.25, 1.57 },
			{ 5.50, 1.41 },
			{ 6.00, 1.23 },
			{ 7.50, 1.02 },
			{ 9.00, 0.93 },
			{ 10.25, 0.91 }, // Approximate end of actual range
	};
	
	private static final double voice_voltage_range = 1.5;
	private static final double voice_DC_voltage = 5.0;
	
	// Capacitor value.
	private static final double C = 470e-12;

	// Transistor parameters.
	private static final double Vdd = 12.18;
	private static final double Vth = 1.31;				// Threshold voltage
	private static final double uCox_vcr = 20e-6;				// 1/2*u*Cox
	private static final double WL_vcr = 9.0/1.0;			// W/L for VCR
	private static final double uCox_snake = 20e-6;			// 1/2*u*Cox
	private static final double WL_snake = 1.0/115.0;		// W/L for "snake"

	// DAC parameters.
	private static final double dac_zero = 6.65;
	private static final double dac_scale = 2.63;
	private static final double dac_2R_div_R = 2.2;
	private static final boolean dac_term = false;
	
	/* Derived stuff */
	private static final double vmin, norm;
	private static final double opamp_working_point;
	private static final double[] dac = new double[11];
	private static final char[] vcr_Vg = new char[1 << 16];
	private static final char[] vcr_n_Ids_term = new char[1 << 16];
	private static final char[] opamp_rev = new char[1 << 16];
	private static final char[][] mixer, summer, gain;
	
	static {
		// Convert op-amp voltage transfer to 16 bit values.
		vmin = opamp_voltage[0][0];
		double vmax = Vdd - Vth;
		norm = 1.0/(vmax - vmin);
		
		double wp = 0;
		for (int i = 0; i < opamp_voltage.length; i ++) {
			if (opamp_voltage[i][0] == opamp_voltage[i][1]) {
				wp = opamp_voltage[i][0];
			}
		}
		opamp_working_point = wp;

		SID.kinkedDac(dac, dac_2R_div_R, dac_term);

		double N16 = norm*((1 << 16) - 1);
		
		// The "zero" output level of the voices.
		// The digital range of one voice is 20 bits; create a scaling term
		// for multiplication which fits in 11 bits.
		double N15 = norm * ((1L << 15) - 1);
		// Create lookup table mapping capacitor voltage to op-amp input voltage:
		// vc -> vx
		double[][] scaled_voltage = new double[opamp_voltage.length][2];
		for (int i = 0; i < opamp_voltage.length; i++) {
			scaled_voltage[i][0] = (N16*(opamp_voltage[i][0] - opamp_voltage[i][1]) + (1 << 16)) / 2;
			scaled_voltage[i][1] = N16*opamp_voltage[i][0];
		}

		Spline s = new Spline(scaled_voltage);
		double[] out = new double[2];
		for (int x = 0; x < 0x10000; x ++) {
			s.evaluate(x, out);
			opamp_rev[x] = (char) (out[0] + 0.5);
		}		
		
		// The filter summer operates at n ~ 1, and has 5 fundamentally different
		// input configurations (2 - 6 input "resistors").
		//
		// Note that all "on" transistors are modeled as one. This is not
		// entirely accurate, since the input for each transistor is different,
		// and transistors are not linear components. However modeling all
		// transistors separately would be extremely costly.
		OpAmp opampModel = new OpAmp(opamp_voltage, Vdd - Vth);
		summer = new char[7][];
		for (int i = 0; i < summer.length; i++) {
			int idiv = 2 + i;        // 2 - 6 input "resistors".
			int size = idiv << 16;
			opampModel.reset();
			summer[i] = new char[size];
			for (int vi = 0; vi < summer[i].length; vi++) {
				double vin = vmin + vi / N16 / idiv; /* vmin .. vmax */
				summer[i][vi] = (char) ((opampModel.solve(idiv, vin) - vmin) * N16 + 0.5);
			}
		}

		// The audio mixer operates at n ~ 8/6, and has 8 fundamentally different
		// input configurations (0 - 7 input "resistors").
		//
		// All "on", transistors are modeled as one - see comments above for
		// the filter summer.
		mixer = new char[8][];
		for (int i = 0; i < mixer.length; i++) {
			final int size;
			if (i == 0) {
				size = 1;
			} else {
				size = i << 16;
			}
			opampModel.reset();
			mixer[i] = new char[size];
			for (int vi = 0; vi < mixer[i].length; vi++) {
				double vin = vmin + vi / N16 / (i == 0 ? 1 : i); /* vmin .. vmax */
				mixer[i][vi] = (char) ((opampModel.solve(i * 8.0/6.0, vin) - vmin) * N16 + 0.5);
			}
		}
		
		// 4 bit "resistor" ladders in the bandpass resonance gain and the audio
		// output gain necessitate 16 gain tables.
		// From die photographs of the bandpass and volume "resistor" ladders
		// it follows that gain ~ vol/8 and 1/Q ~ ~res/8 (assuming ideal
		// op-amps and ideal "resistors").
		gain = new char[16][1 << 16];
		for (int n8 = 0; n8 < FilterModelConfig.gain.length; n8++) {
			opampModel.reset();
			for (int vi = 0; vi < FilterModelConfig.gain[n8].length; vi++) {
				double vin = vmin + vi / N16; /* vmin .. vmax */
				gain[n8][vi] = (char) ((opampModel.solve(n8 / 8.0, vin) - vmin) * N16 + 0.5);
			}
		}
	
		int Vddt = (int) (N16 * (Vdd - Vth) + 0.5);
	    for (int i = 0; i < (1 << 16); i++) {
	        // The table index is right-shifted 16 times in order to fit in
	        // 16 bits; the argument to sqrt is thus multiplied by (1 << 16).
	        int Vg = Vddt - (int) (Math.sqrt((double) i * (1 << 16)) + 0.5);
	        if (Vg >= (1 << 16)) {
	        	// Clamp to 16 bits.
	        	// FIXME: If the DAC output voltage exceeds the max op-amp output
	        	// voltage while the input voltage is at the max op-amp output
	        	// voltage, Vg will not fit in 16 bits.
	        	// Check whether this can happen, and if so, change the lookup table
	        	// to a plain sqrt.
	        	Vg = (1 << 16) - 1;
	        }
	        vcr_Vg[i] = (char) Vg;
	    }

	    double Ut = 26.0e-3;  // Thermal voltage.
	    double k = 1.0;
	    double Is = 2*uCox_vcr*Ut*Ut/k*WL_vcr;
	    // Normalized current factor for 1 cycle at 1MHz.
	    double n_Is = N15*1.0e-6/C*Is;

	    /* 1st term is used for clamping and must therefore be fixed to 0. */
	    vcr_n_Ids_term[0] = 0;
	    for (int Vgx = 1; Vgx < (1 << 16); Vgx++) {
	    	double log_term = Math.log(1 + Math.exp((Vgx/N16 - k*Vth)/(2*Ut)));
	    	// Scaled by m*2^15
	    	vcr_n_Ids_term[Vgx] = (char) (n_Is*log_term*log_term + .5);
	    }
	}
	
	public static double getDacZero(double adjustment) {
		return dac_zero - (adjustment - 0.5) * 2;
	}

	public static int getVO_T16() {
		return (int) (norm * ((1L << 16) - 1) * vmin);
	}

	public static int getVoiceScaleS14() {
		return (int) ((norm * ((1L << 14) - 1)) * voice_voltage_range);
	}

	public static int getVoiceDC() {
		return (int) ((norm * ((1L << 16) - 1)) * (voice_DC_voltage - vmin));
	}

	public static char[][] getGain() {
		return gain;
	}

	public static char[][] getSummer() {
		return summer;
	}

	public static char[][] getMixer() {
		return mixer;
	}
	
	/**
	 * Make DAC
	 */
	public static char[] getDAC(double dac_zero) {
		double N16 = norm * ((1L << 16) - 1);
		final int bits = 11;
		char[] f0_dac = new char[1 << bits];
		for (int i = 0; i < (1 << bits); i++) {
			double fcd = 0;
			for (int j = 0; j < bits; j ++) {
				if ((i & (1 << j)) != 0) {
					fcd += dac[j];
				}
			}
			f0_dac[i] = (char) (N16*(dac_zero + fcd*dac_scale/(1 << bits)) + 0.5);
		}
		return f0_dac;
	}

	public static Integrator buildIntegrator() {
		double N16 = norm * ((1 << 16) - 1);
		int Vddt = (int) (N16 * (Vdd - Vth) + .5);
		int n_snake = (int) ((1 << 13)/norm*(uCox_snake/2*WL_snake*1.0e-6/C) + 0.5);
		return new Integrator(vcr_Vg, vcr_n_Ids_term, opamp_rev, Vddt, n_snake);
	}
		
	private static double evaluateTransistor(double Vw, double vi, double vx) {
		double Vgst = Vdd - Vth - vx;
		double Vgdt = Vdd - Vth - vi;
		double n_snake = uCox_snake/2*WL_snake;
		double n_I_snake = n_snake * (Vgst * Vgst - Vgdt * Vgdt);

		double Vg = Vdd - Vth - Math.sqrt(Math.pow(Vdd - Vth - Vw, 2)/2 + Math.pow(Vgdt, 2)/2);
		double Vgs = Vg - vx;
		double Vgd = Vg - vi;

	    double Ut = 26.0e-3;  // Thermal voltage.
	    double k = 1.0;
	    double Is = 2*uCox_vcr*Ut*Ut/k*WL_vcr;

	    double log_term_f = Math.log(1 + Math.exp((Vgs - k*Vth)/(2*Ut)));
	    double n_I_vcr_f = Is*log_term_f*log_term_f;

	    double log_term_r = Math.log(1 + Math.exp((Vgd - k*Vth)/(2*Ut)));
	    double n_I_vcr_r = Is*log_term_r*log_term_r;

		double n_I_vcr = n_I_vcr_f - n_I_vcr_r;
		return n_I_snake + n_I_vcr;
	}

	/**
	 * Estimate the center frequency corresponding to some FC setting.
	 * 
	 * FIXME: this function is extremely sensitive to prevailing voltage offsets.
	 * They got to be right within about 0.1V, or the results will be simply wrong.
	 * This casts doubt on the feasibility of this approach. Perhaps the offsets
	 * at the integrators would need to be statically solved first for 1-voice null
	 * input.
	 * 
	 * @param fc
	 * @return frequency in Hz
	 */
	public static double estimateFrequency(double dac_zero, int fc) {
		/* Calculate input from DAC */
		int bits = 11;
		double Vw = 0;
		for (int j = 0; j < bits; j ++) {
			if ((fc & (1 << j)) != 0) {
				Vw += dac[j];
			}
		}
		Vw = dac_zero + dac_scale * Vw / (1 << bits);

		/* Estimate the behavior for small signals around the op-amp working point. */
		double vx = opamp_working_point;
		double diff = 0.2;
		double n_I = 0;
		n_I -= evaluateTransistor(Vw, vx - diff, vx);
		n_I += evaluateTransistor(Vw, vx + diff, vx);
		n_I /= 2;

		/* Convert the current to frequency based on the calculated current and the potential. */
		return n_I / (2 * Math.PI * C * diff);
	}
}