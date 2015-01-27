/**
 * This file is part of reSID, a MOS6581 SID emulator engine.
 * Copyright (C) 2004  Dag Lem <resid@nimrod.no>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 * @author Ken Handel
 *
 */
package ucesoft.c64.peripheral.sid.resid;

/**
 * The SID filter is modeled with a two-integrator-loop biquadratic filter,
 * which has been confirmed by Bob Yannes to be the actual circuit used in the
 * SID chip.
 * <P>
 * Measurements show that excellent emulation of the SID filter is achieved,
 * except when high resonance is combined with high sustain levels. In this case
 * the SID op-amps are performing less than ideally and are causing some
 * peculiar behavior of the SID filter. This however seems to have more effect
 * on the overall amplitude than on the color of the sound.
 * <P>
 * The theory for the filter circuit can be found in "Microelectric Circuits" by
 * Adel S. Sedra and Kenneth C. Smith. The circuit is modeled based on the
 * explanation found there except that an additional inverter is used in the
 * feedback from the bandpass output, allowing the summer op-amp to operate in
 * single-ended mode. This yields inverted filter outputs with levels
 * independent of Q, which corresponds with the results obtained from a real
 * SID.
 * <P>
 * We have been able to model the summer and the two integrators of the circuit
 * to form components of an IIR filter. Vhp is the output of the summer, Vbp is
 * the output of the first integrator, and Vlp is the output of the second
 * integrator in the filter circuit.
 * <P>
 * According to Bob Yannes, the active stages of the SID filter are not really
 * op-amps. Rather, simple NMOS inverters are used. By biasing an inverter into
 * its region of quasi-linear operation using a feedback resistor from input to
 * output, a MOS inverter can be made to act like an op-amp for small signals
 * centered around the switching threshold.
 * <P>
 * Qualified guesses at SID filter schematics are depicted below.
 * 
 * <pre>
 * SID filter
 * ----------
 * 
 *     -----------------------------------------------
 *    |                                               |
 *    |            ---Rq--                            |
 *    |           |       |                           |
 *    |  ------------&lt;A]-----R1---------              |
 *    | |                               |             |
 *    | |                        ---C---|      ---C---|
 *    | |                       |       |     |       |
 *    |  --R1--    ---R1--      |---Rs--|     |---Rs--| 
 *    |        |  |       |     |       |     |       |
 *     ----R1--|-----[A&gt;--|--R-----[A&gt;--|--R-----[A&gt;--|
 *             |          |             |             |
 * vi -----R1--           |             |             |
 * 
 *                       vhp           vbp           vlp
 * 
 * 
 * vi  - input voltage
 * vhp - highpass output
 * vbp - bandpass output
 * vlp - lowpass output
 * [A&gt; - op-amp
 * R1  - summer resistor
 * Rq  - resistor array controlling resonance (4 resistors)
 * R   - NMOS FET voltage controlled resistor controlling cutoff frequency
 * Rs  - shunt resitor
 * C   - capacitor
 * 
 * 
 * 
 * SID integrator
 * --------------
 * 
 *                                   V+
 * 
 *                                   |
 *                                   |
 *                              -----|
 *                             |     |
 *                             | ||--
 *                              -||
 *                   ---C---     ||-&gt;
 *                  |       |        |
 *                  |---Rs-----------|---- vo
 *                  |                |
 *                  |            ||--
 * vi ----     -----|------------||
 *        |   &circ;     |            ||-&gt;
 *        |___|     |                |
 *        -----     |                |
 *          |       |                |
 *          |---R2--                 |
 *          |
 *          R1                       V-
 *          |
 *          |
 * 
 *          Vw
 * ----------------------------------------------------------------------------
 * </pre>
 * 
 * @author Ken Handel
 */
public class Filter {

	/**
	 * #define SPLINE_BRUTE_FORCE false
	 */
	public static final boolean SPLINE_BRUTE_FORCE = false;

	/**
	 * Filter enabled.
	 */
	protected boolean enabled;

	/**
	 * Filter cutoff frequency.
	 */
	protected int /* reg12 */fc;

	/**
	 * Filter resonance.
	 */
	protected int /* reg8 */res;

	/**
	 * Selects which inputs to route through filter.
	 */
	protected int /* reg8 */filt;

	/**
	 * Switch voice 3 off.
	 */
	protected int /* reg8 */voice3off;

	/**
	 * Highpass, bandpass, and lowpass filter modes.
	 */
	protected int /* reg8 */hp_bp_lp;

	/**
	 * Output master volume.
	 */
	protected int /* reg4 */vol;

	/**
	 * Mixer DC offset.
	 */
	protected int /* sound_sample */mixer_DC;

	/**
	 * State of filter. highpass
	 */
	protected int /* sound_sample */Vhp;

	/**
	 * State of filter. bandpass
	 */
	protected int /* sound_sample */Vbp;

	/**
	 * State of filter. lowpass
	 */
	protected int /* sound_sample */Vlp;

	/**
	 * State of filter. not filtered
	 */
	protected int /* sound_sample */Vnf;

	/**
	 * when to begin, how fast it grows
	 */
	int /* sound_sample */DLthreshold, DLsteepness;
	int /* sound_sample */DHthreshold, DHsteepness;
	int /* sound_sample */DLlp, DLbp, DLhp; // coefficients, 256 = 1.0
	int /* sound_sample */DHlp, DHbp, DHhp;

	/**
	 * Cutoff frequency, resonance.
	 */
	protected int /* sound_sample */w0, w0_ceil_1, w0_ceil_dt;

	protected int /* sound_sample */_1024_div_Q;

	/**
	 * Cutoff frequency tables. FC is an 11 bit register.
	 */
	protected int /* sound_sample */f0_6581[] = new int[2048];

	/**
	 * Cutoff frequency tables. FC is an 11 bit register.
	 */
	protected int /* sound_sample */f0_8580[] = new int[2048];

	protected int /* sound_sample */f0[];

	/**
	 * 
	 * Maximum cutoff frequency is specified as FCmax = 2.6e-5/C =
	 * 2.6e-5/2200e-12 = 11818.
	 * <P>
	 * Measurements indicate a cutoff frequency range of approximately 220Hz -
	 * 18kHz on a MOS6581 fitted with 470pF capacitors. The function mapping FC
	 * to cutoff frequency has the shape of the tanh function, with a
	 * discontinuity at FCHI = 0x80. In contrast, the MOS8580 almost perfectly
	 * corresponds with the specification of a linear mapping from 30Hz to
	 * 12kHz.
	 * <P>
	 * The mappings have been measured by feeding the SID with an external
	 * signal since the chip itself is incapable of generating waveforms of
	 * higher fundamental frequency than 4kHz. It is best to use the bandpass
	 * output at full resonance to pick out the cutoff frequency at any given FC
	 * setting.
	 * <P>
	 * The mapping function is specified with spline interpolation points and
	 * the function values are retrieved via table lookup.
	 * <P>
	 * NB! Cutoff frequency characteristics may vary, we have modeled two
	 * particular Commodore 64s.
	 */
	protected static int[] /* fc_point */f0_points_6581[] = {
	// -----FC----f-------FCHI-FCLO
			// ----------------------------
			{ 0, 220 }, // 0x00 - repeated end point
			{ 0, 220 }, // 0x00
			{ 128, 230 }, // 0x10
			{ 256, 250 }, // 0x20
			{ 384, 300 }, // 0x30
			{ 512, 420 }, // 0x40
			{ 640, 780 }, // 0x50
			{ 768, 1600 }, // 0x60
			{ 832, 2300 }, // 0x68
			{ 896, 3200 }, // 0x70
			{ 960, 4300 }, // 0x78
			{ 992, 5000 }, // 0x7c
			{ 1008, 5400 }, // 0x7e
			{ 1016, 5700 }, // 0x7f
			{ 1023, 6000 }, // 0x7f 0x07
			{ 1023, 6000 }, // 0x7f 0x07 - discontinuity
			{ 1024, 4600 }, // 0x80 -
			{ 1024, 4600 }, // 0x80
			{ 1032, 4800 }, // 0x81
			{ 1056, 5300 }, // 0x84
			{ 1088, 6000 }, // 0x88
			{ 1120, 6600 }, // 0x8c
			{ 1152, 7200 }, // 0x90
			{ 1280, 9500 }, // 0xa0
			{ 1408, 12000 }, // 0xb0
			{ 1536, 14500 }, // 0xc0
			{ 1664, 16000 }, // 0xd0
			{ 1792, 17100 }, // 0xe0
			{ 1920, 17700 }, // 0xf0
			{ 2047, 18000 }, // 0xff 0x07
			{ 2047, 18000 } // 0xff 0x07 - repeated end point
	};

	/**
	 * 
	 * Maximum cutoff frequency is specified as FCmax = 2.6e-5/C =
	 * 2.6e-5/2200e-12 = 11818.
	 * 
	 * Measurements indicate a cutoff frequency range of approximately 220Hz -
	 * 18kHz on a MOS6581 fitted with 470pF capacitors. The function mapping FC
	 * to cutoff frequency has the shape of the tanh function, with a
	 * discontinuity at FCHI = 0x80. In contrast, the MOS8580 almost perfectly
	 * corresponds with the specification of a linear mapping from 30Hz to
	 * 12kHz.
	 * 
	 * The mappings have been measured by feeding the SID with an external
	 * signal since the chip itself is incapable of generating waveforms of
	 * higher fundamental frequency than 4kHz. It is best to use the bandpass
	 * output at full resonance to pick out the cutoff frequency at any given FC
	 * setting.
	 * 
	 * The mapping function is specified with spline interpolation points and
	 * the function values are retrieved via table lookup.
	 * 
	 * NB! Cutoff frequency characteristics may vary, we have modeled two
	 * particular Commodore 64s.
	 */
	protected static int[] /* fc_point */f0_points_8580[] = {
	// -----FC----f-------FCHI-FCLO
			// ----------------------------
			{ 0, 0 }, // 0x00 - repeated end point
			{ 0, 0 }, // 0x00
			{ 128, 800 }, // 0x10
			{ 256, 1600 }, // 0x20
			{ 384, 2500 }, // 0x30
			{ 512, 3300 }, // 0x40
			{ 640, 4100 }, // 0x50
			{ 768, 4800 }, // 0x60
			{ 896, 5600 }, // 0x70
			{ 1024, 6500 }, // 0x80
			{ 1152, 7500 }, // 0x90
			{ 1280, 8400 }, // 0xa0
			{ 1408, 9200 }, // 0xb0
			{ 1536, 9800 }, // 0xc0
			{ 1664, 10500 }, // 0xd0
			{ 1792, 11000 }, // 0xe0
			{ 1920, 11700 }, // 0xf0
			{ 2047, 12500 }, // 0xff 0x07
			{ 2047, 12500 } // 0xff 0x07 - repeated end point
	};

	protected int[] /* fc_point */f0_points[];

	protected int f0_count;

	// ----------------------------------------------------------------------------
	// Inline functions.
	// The following functions are defined inline because they are called every
	// time a sample is calculated.
	// ----------------------------------------------------------------------------

	/**
	 * SID clocking - 1 cycle
	 * 
	 * @param voice1
	 * @param voice2
	 * @param voice3
	 * @param ext_in
	 */
	public void clock(int /* sound_sample */voice1,
			int /* sound_sample */voice2, int /* sound_sample */voice3,
			int /* sound_sample */ext_in) {
		// Scale each voice down from 20 to 13 bits.
		voice1 >>= 7;
		voice2 >>= 7;

		// NB! Voice 3 is not silenced by voice3off if it is routed through
		// the filter.
		if ((voice3off != 0) && ((filt & 0x04) == 0)) {
			voice3 = 0;
		} else {
			voice3 >>= 7;
		}

		ext_in >>= 7;

		// This is handy for testing.
		if (!enabled) {
			Vnf = voice1 + voice2 + voice3 + ext_in;
			Vhp = Vbp = Vlp = 0;
			return;
		}

		int /* sound_sample */Vi = Vnf = 0;
		// Route voices into or around filter.

		if (SID.ANTTI_LANKILA_PATCH) {

			if ((filt & 1) != 0)
				Vi += voice1;
			else
				Vnf += voice1;
			if ((filt & 2) != 0)
				Vi += voice2;
			else
				Vnf += voice2;
			if ((filt & 4) != 0)
				Vi += voice3;
			else
				Vnf += voice3;
			if ((filt & 8) != 0)
				Vi += ext_in;
			else
				Vnf += ext_in;
		} else {
			// The code below is expanded to a switch for faster execution.
			// (filt1 ? Vi : Vnf) += voice1;
			// (filt2 ? Vi : Vnf) += voice2;
			// (filt3 ? Vi : Vnf) += voice3;

			switch (filt) {
			default:
			case 0x0:
				Vi = 0;
				Vnf = voice1 + voice2 + voice3 + ext_in;
				break;
			case 0x1:
				Vi = voice1;
				Vnf = voice2 + voice3 + ext_in;
				break;
			case 0x2:
				Vi = voice2;
				Vnf = voice1 + voice3 + ext_in;
				break;
			case 0x3:
				Vi = voice1 + voice2;
				Vnf = voice3 + ext_in;
				break;
			case 0x4:
				Vi = voice3;
				Vnf = voice1 + voice2 + ext_in;
				break;
			case 0x5:
				Vi = voice1 + voice3;
				Vnf = voice2 + ext_in;
				break;
			case 0x6:
				Vi = voice2 + voice3;
				Vnf = voice1 + ext_in;
				break;
			case 0x7:
				Vi = voice1 + voice2 + voice3;
				Vnf = ext_in;
				break;
			case 0x8:
				Vi = ext_in;
				Vnf = voice1 + voice2 + voice3;
				break;
			case 0x9:
				Vi = voice1 + ext_in;
				Vnf = voice2 + voice3;
				break;
			case 0xa:
				Vi = voice2 + ext_in;
				Vnf = voice1 + voice3;
				break;
			case 0xb:
				Vi = voice1 + voice2 + ext_in;
				Vnf = voice3;
				break;
			case 0xc:
				Vi = voice3 + ext_in;
				Vnf = voice1 + voice2;
				break;
			case 0xd:
				Vi = voice1 + voice3 + ext_in;
				Vnf = voice2;
				break;
			case 0xe:
				Vi = voice2 + voice3 + ext_in;
				Vnf = voice1;
				break;
			case 0xf:
				Vi = voice1 + voice2 + voice3 + ext_in;
				Vnf = 0;
				break;
			}
		}

		// delta_t = 1 is converted to seconds given a 1MHz clock by dividing
		// with 1 000 000.

		if (SID.ANTTI_LANKILA_PATCH) {
			int /* sound_sample */Vi_peak_bp = ((Vlp * DHlp + Vbp * DHbp + Vhp
					* DHhp) >> 8)
					+ Vi;
			if (Vi_peak_bp < DHthreshold)
				Vi_peak_bp = DHthreshold;
			int /* sound_sample */Vi_peak_lp = ((Vlp * DLlp + Vbp * DLbp + Vhp
					* DLhp) >> 8)
					+ Vi;
			if (Vi_peak_lp < DLthreshold)
				Vi_peak_lp = DLthreshold;
			int /* sound_sample */w0_eff_bp = w0 + w0
					* ((Vi_peak_bp - DHthreshold) >> 4) / DHsteepness;
			int /* sound_sample */w0_eff_lp = w0 + w0
					* ((Vi_peak_lp - DLthreshold) >> 4) / DLsteepness;
			/* we need to ensure filter's stability. */
			if (w0_eff_bp > w0_ceil_1)
				w0_eff_bp = w0_ceil_1;
			if (w0_eff_lp > w0_ceil_1)
				w0_eff_lp = w0_ceil_1;

			Vhp = (Vbp * _1024_div_Q >> 10) - Vlp - Vi;
			Vlp -= w0_eff_lp * Vbp >> 20;
			Vbp -= w0_eff_bp * Vhp >> 20;
		} else {
			// Calculate filter outputs.
			// Vhp = Vbp/Q - Vlp - Vi;
			// dVbp = -w0*Vhp*dt;
			// dVlp = -w0*Vbp*dt;

			int /* sound_sample */dVbp = (w0_ceil_1 * Vhp >> 20);
			int /* sound_sample */dVlp = (w0_ceil_1 * Vbp >> 20);
			Vbp -= dVbp;
			Vlp -= dVlp;
			Vhp = (Vbp * _1024_div_Q >> 10) - Vlp - Vi;
		}
	}

	/**
	 * SID clocking - delta_t cycles.
	 * 
	 * @param delta_t
	 * @param voice1
	 * @param voice2
	 * @param voice3
	 * @param ext_in
	 */
	public void clock(int /* cycle_count */delta_t,
			int /* sound_sample */voice1, int /* sound_sample */voice2,
			int /* sound_sample */voice3, int /* sound_sample */ext_in) {
		// Scale each voice down from 20 to 13 bits.
		voice1 >>= 7;
		voice2 >>= 7;

		// NB! Voice 3 is not silenced by voice3off if it is routed through
		// the filter.
		if ((voice3off != 0) && ((filt & 0x04) == 0)) {
			voice3 = 0;
		} else {
			voice3 >>= 7;
		}

		ext_in >>= 7;

		// Enable filter on/off.
		// This is not really part of SID, but is useful for testing.
		// On slow CPUs it may be necessary to bypass the filter to lower the
		// CPU load.
		if (!enabled) {
			Vnf = voice1 + voice2 + voice3 + ext_in;
			Vhp = Vbp = Vlp = 0;
			return;
		}

		int /* sound_sample */Vi = Vnf = 0;

		// Route voices into or around filter.
		// The code below is expanded to a switch for faster execution.
		if (!SID.ANTTI_LANKILA_PATCH) {
			// (filt1 ? Vi : Vnf) += voice1;
			// (filt2 ? Vi : Vnf) += voice2;
			// (filt3 ? Vi : Vnf) += voice3;

			switch (filt) {
			default:
			case 0x0:
				Vi = 0;
				Vnf = voice1 + voice2 + voice3 + ext_in;
				break;
			case 0x1:
				Vi = voice1;
				Vnf = voice2 + voice3 + ext_in;
				break;
			case 0x2:
				Vi = voice2;
				Vnf = voice1 + voice3 + ext_in;
				break;
			case 0x3:
				Vi = voice1 + voice2;
				Vnf = voice3 + ext_in;
				break;
			case 0x4:
				Vi = voice3;
				Vnf = voice1 + voice2 + ext_in;
				break;
			case 0x5:
				Vi = voice1 + voice3;
				Vnf = voice2 + ext_in;
				break;
			case 0x6:
				Vi = voice2 + voice3;
				Vnf = voice1 + ext_in;
				break;
			case 0x7:
				Vi = voice1 + voice2 + voice3;
				Vnf = ext_in;
				break;
			case 0x8:
				Vi = ext_in;
				Vnf = voice1 + voice2 + voice3;
				break;
			case 0x9:
				Vi = voice1 + ext_in;
				Vnf = voice2 + voice3;
				break;
			case 0xa:
				Vi = voice2 + ext_in;
				Vnf = voice1 + voice3;
				break;
			case 0xb:
				Vi = voice1 + voice2 + ext_in;
				Vnf = voice3;
				break;
			case 0xc:
				Vi = voice3 + ext_in;
				Vnf = voice1 + voice2;
				break;
			case 0xd:
				Vi = voice1 + voice3 + ext_in;
				Vnf = voice2;
				break;
			case 0xe:
				Vi = voice2 + voice3 + ext_in;
				Vnf = voice1;
				break;
			case 0xf:
				Vi = voice1 + voice2 + voice3 + ext_in;
				Vnf = 0;
				break;
			}
		} else {
			if ((filt & 1) != 0)
				Vi += voice1;
			else
				Vnf += voice1;
			if ((filt & 2) != 0)
				Vi += voice2;
			else
				Vnf += voice2;
			if ((filt & 4) != 0)
				Vi += voice3;
			else
				Vnf += voice3;
			if ((filt & 8) != 0)
				Vi += ext_in;
			else
				Vnf += ext_in;
		}
		// Maximum delta cycles for the filter to work satisfactorily under
		// current
		// cutoff frequency and resonance constraints is approximately 8.
		int /* cycle_count */delta_t_flt = 8;

		while (delta_t != 0) {
			if (delta_t < delta_t_flt) {
				delta_t_flt = delta_t;
			}

			// delta_t is converted to seconds given a 1MHz clock by dividing
			// with 1 000 000. This is done in two operations to avoid integer
			// multiplication overflow.

			// Calculate filter outputs.
			// Vhp = Vbp/Q - Vlp - Vi;
			// dVbp = -w0*Vhp*dt;
			// dVlp = -w0*Vbp*dt;
			int /* sound_sample */w0_delta_t = w0_ceil_dt * delta_t_flt >> 6;

			int /* sound_sample */dVbp = (w0_delta_t * Vhp >> 14);
			int /* sound_sample */dVlp = (w0_delta_t * Vbp >> 14);
			Vbp -= dVbp;
			Vlp -= dVlp;
			Vhp = (Vbp * _1024_div_Q >> 10) - Vlp - Vi;

			delta_t -= delta_t_flt;
		}
	}

	/**
	 * SID audio output (16 bits). SID audio output (20 bits).
	 * 
	 * @return
	 */
	public int /* sound_sample */output() {
		// This is handy for testing.
		if (!enabled) {
			return (Vnf + mixer_DC) * /* static_cast<sound_sample> */(vol);
		}

		if (!SID.ANTTI_LANKILA_PATCH) {
			// Mix highpass, bandpass, and lowpass outputs. The sum is not
			// weighted, this can be confirmed by sampling sound output for
			// e.g. bandpass, lowpass, and bandpass+lowpass from a SID chip.

			// The code below is expanded to a switch for faster execution.
			// if (hp) Vf += Vhp;
			// if (bp) Vf += Vbp;
			// if (lp) Vf += Vlp;

			int /* sound_sample */Vf;

			switch (hp_bp_lp) {
			default:
			case 0x0:
				Vf = 0;
				break;
			case 0x1:
				Vf = Vlp;
				break;
			case 0x2:
				Vf = Vbp;
				break;
			case 0x3:
				Vf = Vlp + Vbp;
				break;
			case 0x4:
				Vf = Vhp;
				break;
			case 0x5:
				Vf = Vlp + Vhp;
				break;
			case 0x6:
				Vf = Vbp + Vhp;
				break;
			case 0x7:
				Vf = Vlp + Vbp + Vhp;
				break;
			}

			// Sum non-filtered and filtered output.
			// Multiply the sum with volume.
			return (Vnf + Vf + mixer_DC)
					* /* static_cast<sound_sample> */(vol);
		} else {
			int /* sound_sample */Vf = 0;
			if ((hp_bp_lp & 1) != 0)
				Vf += Vlp;
			if ((hp_bp_lp & 2) != 0)
				Vf += Vbp;
			if ((hp_bp_lp & 4) != 0)
				Vf += Vhp;

			// Sum non-filtered and filtered output.
			// Multiply the sum with volume.
			return (Vnf + Vf + mixer_DC)
					* /* static_cast<sound_sample> */(vol);
		}
	}

	// ----------------------------------------------------------------------------
	// END Inline functions.
	// ----------------------------------------------------------------------------

	/**
	 * Constructor.
	 */
	public Filter() {
		fc = 0;

		res = 0;

		filt = 0;

		voice3off = 0;

		hp_bp_lp = 0;

		vol = 0;

		// State of filter.
		Vhp = 0;
		Vbp = 0;
		Vlp = 0;
		Vnf = 0;

		enable_filter(true);

		// Create mappings from FC to cutoff frequency.
		interpolate(f0_points_6581, 0, f0_points_6581.length - 1,
				new PointPlotter(f0_6581), 1.0);
		interpolate(f0_points_8580, 0, f0_points_8580.length - 1,
				new PointPlotter(f0_8580), 1.0);

		set_chip_model(ISIDDefs.chip_model.MOS6581);

		/* no distortion by default */
		set_distortion_properties(999999, 999999, 0, 0, 0, 999999, 999999, 0,
				0, 0);
	}

	/**
	 * Enable filter.
	 * 
	 * @param enable
	 */
	public void enable_filter(boolean enable) {
		enabled = enable;
	}

	/**
	 * Set chip model.
	 * 
	 * @param model
	 */
	public void set_chip_model(ISIDDefs.chip_model model) {
		if (model == ISIDDefs.chip_model.MOS6581) {
			// The mixer has a small input DC offset. This is found as follows:
			//
			// The "zero" output level of the mixer measured on the SID audio
			// output pin is 5.50V at zero volume, and 5.44 at full
			// volume. This yields a DC offset of (5.44V - 5.50V) = -0.06V.
			//
			// The DC offset is thus -0.06V/1.05V ~ -1/18 of the dynamic range
			// of one voice. See Voice.java for measurement of the dynamic
			// range.

			mixer_DC = -0xfff * 0xff / 18 >> 7;

			f0 = f0_6581;
			f0_points = f0_points_6581;
			f0_count = f0_points_6581.length;
		} else {
			// No DC offsets in the MOS8580.
			mixer_DC = 0;

			f0 = f0_8580;
			f0_points = f0_points_8580;
			f0_count = f0_points_8580.length;
		}

		set_w0();
		set_Q();
	}

	void set_distortion_properties(int Lthreshold, int Lsteepness, int Llp,
			int Lbp, int Lhp, int Hthreshold, int Hsteepness, int Hlp, int Hbp,
			int Hhp) {
		DLthreshold = Lthreshold;
		if (Lsteepness < 16)
			Lsteepness = 16; /* avoid division by zero */
		DLsteepness = Lsteepness >> 4;
		DLlp = Llp;
		DLbp = Lbp;
		DLhp = Lhp;

		DHthreshold = Hthreshold;
		if (Hsteepness < 16)
			Hsteepness = 16;
		DHsteepness = Hsteepness >> 4;
		DHlp = Hlp;
		DHbp = Hbp;
		DHhp = Hhp;
	}

	/**
	 * SID reset.
	 */
	public void reset() {
		fc = 0;

		res = 0;

		filt = 0;

		voice3off = 0;

		hp_bp_lp = 0;

		vol = 0;

		// State of filter.
		Vhp = 0;
		Vbp = 0;
		Vlp = 0;
		Vnf = 0;

		set_w0();
		set_Q();
	}

	/**
	 * Register functions.
	 * 
	 * @param fc_lo
	 */
	public void writeFC_LO(int /* reg8 */fc_lo) {
		fc = fc & 0x7f8 | fc_lo & 0x007;
		set_w0();
	}

	/**
	 * Register functions.
	 * 
	 * @param fc_hi
	 */
	public void writeFC_HI(int /* reg8 */fc_hi) {
		fc = (fc_hi << 3) & 0x7f8 | fc & 0x007;
		set_w0();
	}

	/**
	 * Register functions.
	 * 
	 * @param res_filt
	 */
	public void writeRES_FILT(int /* reg8 */res_filt) {
		res = (res_filt >> 4) & 0x0f;
		set_Q();

		filt = res_filt & 0x0f;
	}

	/**
	 * Register functions.
	 * 
	 * @param mode_vol
	 */
	public void writeMODE_VOL(int /* reg8 */mode_vol) {
		voice3off = mode_vol & 0x80;

		hp_bp_lp = (mode_vol >> 4) & 0x07;

		vol = mode_vol & 0x0f;
	}

	// Set filter cutoff frequency.
	protected void set_w0() {
		final double pi = 3.1415926535897932385;

		// Multiply with 1.048576 to facilitate division by 1 000 000 by right-
		// shifting 20 times (2 ^ 20 = 1048576).
		w0 = (int) /* static_cast<sound_sample> */(2 * pi * f0[fc] * 1.048576);

		if (SID.ANTTI_LANKILA_PATCH) {
			// Set the static limit to the dynamic, distortion-driven filter.
			// I need a few kHz headroom at least to be even half certain that
			// the
			// filter will not drive itself to oblivion.
			w0_ceil_1 = (int) /* static_cast<sound_sample> */(2 * pi * 18000 * 1.048576);
		} else {
			// Limit f0 to 16kHz to keep 1 cycle filter stable.
			final int /* sound_sample */w0_max_1 = (int) /* static_cast<sound_sample> */(2 * pi * 16000 * 1.048576);
			w0_ceil_1 = w0 <= w0_max_1 ? w0 : w0_max_1;
		}

		// Limit f0 to 4kHz to keep delta_t cycle filter stable.
		final int /* sound_sample */w0_max_dt = (int) /* static_cast<sound_sample> */(2 * pi * 4000 * 1.048576);
		w0_ceil_dt = w0 <= w0_max_dt ? w0 : w0_max_dt;
	}

	/**
	 * Set filter resonance.
	 */
	protected void set_Q() {
		// Q is controlled linearly by res. Q has approximate range [0.707,
		// 1.7].
		// As resonance is increased, the filter must be clocked more often to
		// keep stable.

		// The coefficient 1024 is dispensed of later by right-shifting 10 times
		// (2 ^ 10 = 1024).
		_1024_div_Q = (int) /* static_cast<sound_sample> */(1024.0 / (0.707 + 1.0 * res / 0x0f));
	}

	// ----------------------------------------------------------------------------
	// Spline functions.
	// ----------------------------------------------------------------------------

	/**
	 * Return the array of spline interpolation points used to map the FC
	 * register to filter cutoff frequency.
	 * 
	 * @param fcp
	 *            IN/OUT parameter points and count
	 */
	public void fc_default(SID.FCPoints fcp) {
		fcp.points = f0_points;
		fcp.count = f0_count;
	}

	// ----------------------------------------------------------------------------
	// Given an array of interpolation points p with n points, the following
	// statement will specify a new FC mapping:
	// interpolate(p, p + n - 1, filter.fc_plotter(), 1.0);
	// Note that the x range of the interpolation points *must* be [0, 2047],
	// and that additional end points *must* be present since the end points
	// are not interpolated.
	// ----------------------------------------------------------------------------
	public PointPlotter fc_plotter() {
		return new PointPlotter(f0);
	}

	// Our objective is to construct a smooth interpolating single-valued
	// function
	// y = f(x).
	//
	// Catmull-Rom splines are widely used for interpolation, however these are
	// parametric curves [x(t) y(t) ...] and can not be used to directly
	// calculate
	// y = f(x).
	// For a discussion of Catmull-Rom splines see Catmull, E., and R. Rom,
	// "A Class of Local Interpolating Splines", Computer Aided Geometric
	// Design.
	//
	// Natural cubic splines are single-valued functions, and have been used in
	// several applications e.g. to specify gamma curves for image display.
	// These splines do not afford local control, and a set of linear equations
	// including all interpolation points must be solved before any point on the
	// curve can be calculated. The lack of local control makes the splines
	// more difficult to handle than e.g. Catmull-Rom splines, and real-time
	// interpolation of a stream of data points is not possible.
	// For a discussion of natural cubic splines, see e.g. Kreyszig, E.,
	// "Advanced
	// Engineering Mathematics".
	//
	// Our approach is to approximate the properties of Catmull-Rom splines for
	// piecewice cubic polynomials f(x) = ax^3 + bx^2 + cx + d as follows:
	// Each curve segment is specified by four interpolation points,
	// p0, p1, p2, p3.
	// The curve between p1 and p2 must interpolate both p1 and p2, and in
	// addition
	// f'(p1.x) = k1 = (p2.y - p0.y)/(p2.x - p0.x) and
	// f'(p2.x) = k2 = (p3.y - p1.y)/(p3.x - p1.x).
	//
	// The constraints are expressed by the following system of linear equations
	//
	// [ 1 xi xi^2 xi^3 ] [ d ] [ yi ]
	// [ 1 2*xi 3*xi^2 ] * [ c ] = [ ki ]
	// [ 1 xj xj^2 xj^3 ] [ b ] [ yj ]
	// [ 1 2*xj 3*xj^2 ] [ a ] [ kj ]
	//
	// Solving using Gaussian elimination and back substitution, setting
	// dy = yj - yi, dx = xj - xi, we get
	//	 
	// a = ((ki + kj) - 2*dy/dx)/(dx*dx);
	// b = ((kj - ki)/dx - 3*(xi + xj)*a)/2;
	// c = ki - (3*xi*a + 2*b)*xi;
	// d = yi - ((xi*a + b)*xi + c)*xi;
	//
	// Having calculated the coefficients of the cubic polynomial we have the
	// choice of evaluation by brute force
	//
	// for (x = x1; x <= x2; x += res) {
	// y = ((a*x + b)*x + c)*x + d;
	// plot(x, y);
	// }
	//
	// or by forward differencing
	//
	// y = ((a*x1 + b)*x1 + c)*x1 + d;
	// dy = (3*a*(x1 + res) + 2*b)*x1*res + ((a*res + b)*res + c)*res;
	// d2y = (6*a*(x1 + res) + 2*b)*res*res;
	// d3y = 6*a*res*res*res;
	//	     
	// for (x = x1; x <= x2; x += res) {
	// plot(x, y);
	// y += dy; dy += d2y; d2y += d3y;
	// }
	//
	// See Foley, Van Dam, Feiner, Hughes, "Computer Graphics, Principles and
	// Practice" for a discussion of forward differencing.
	//
	// If we have a set of interpolation points p0, ..., pn, we may specify
	// curve segments between p0 and p1, and between pn-1 and pn by using the
	// following constraints:
	// f''(p0.x) = 0 and
	// f''(pn.x) = 0.
	//
	// Substituting the results for a and b in
	//
	// 2*b + 6*a*xi = 0
	//
	// we get
	//
	// ki = (3*dy/dx - kj)/2;
	//
	// or by substituting the results for a and b in
	//
	// 2*b + 6*a*xj = 0
	//
	// we get
	//
	// kj = (3*dy/dx - ki)/2;
	//
	// Finally, if we have only two interpolation points, the cubic polynomial
	// will degenerate to a straight line if we set
	//
	// ki = kj = dy/dx;
	//

	public class Coefficients {

		public double a;

		public double b;

		public double c;

		public double d;
	}

	/**
	 * Calculation of coefficients.
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 * @param k1
	 * @param k2
	 * @param coeff
	 */
	protected void cubic_coefficients(double x1, double y1, double x2,
			double y2, double k1, double k2, Coefficients coeff) {
		double dx = x2 - x1, dy = y2 - y1;

		coeff.a = ((k1 + k2) - 2 * dy / dx) / (dx * dx);
		coeff.b = ((k2 - k1) / dx - 3 * (x1 + x2) * coeff.a) / 2;
		coeff.c = k1 - (3 * x1 * coeff.a + 2 * coeff.b) * x1;
		coeff.d = y1 - ((x1 * coeff.a + coeff.b) * x1 + coeff.c) * x1;
	}

	/**
	 * Evaluation of cubic polynomial by brute force.
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 * @param k1
	 * @param k2
	 * @param plotter
	 * @param res
	 */
	protected void interpolate_brute_force(double x1, double y1, double x2,
			double y2, double k1, double k2, PointPlotter plotter, double res) {
		Coefficients coeff = new Coefficients();
		cubic_coefficients(x1, y1, x2, y2, k1, k2, coeff);

		// Calculate each point.
		for (double x = x1; x <= x2; x += res) {
			double y = ((coeff.a * x + coeff.b) * x + coeff.c) * x + coeff.d;
			plotter.plot(x, y);
		}
	}

	/**
	 * Evaluation of cubic polynomial by forward differencing.
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 * @param k1
	 * @param k2
	 * @param plotter
	 * @param res
	 */
	protected void interpolate_forward_difference(double x1, double y1,
			double x2, double y2, double k1, double k2, PointPlotter plotter,
			double res) {
		Coefficients coeff = new Coefficients();
		cubic_coefficients(x1, y1, x2, y2, k1, k2, coeff);

		double y = ((coeff.a * x1 + coeff.b) * x1 + coeff.c) * x1 + coeff.d;
		double dy = (3 * coeff.a * (x1 + res) + 2 * coeff.b) * x1 * res
				+ ((coeff.a * res + coeff.b) * res + coeff.c) * res;
		double d2y = (6 * coeff.a * (x1 + res) + 2 * coeff.b) * res * res;
		double d3y = 6 * coeff.a * res * res * res;

		// Calculate each point.
		for (double x = x1; x <= x2; x += res) {
			plotter.plot(x, y);
			y += dy;
			dy += d2y;
			d2y += d3y;
		}
	}

	protected double x(int[] /* fc_point */f0_base[], int p) {
		return (f0_base[p])[0];
	}

	protected double y(int[] /* fc_point */f0_base[], int p) {
		return (f0_base[p])[1];
	}

	/**
	 * Evaluation of complete interpolating function. Note that since each curve
	 * segment is controlled by four points, the end points will not be
	 * interpolated. If extra control points are not desirable, the end points
	 * can simply be repeated to ensure interpolation. Note also that points of
	 * non-differentiability and discontinuity can be introduced by repeating
	 * points.
	 * 
	 * @param p0
	 * @param pn
	 * @param plotter
	 * @param res
	 */
	public void interpolate(int[] /* fc_point */f0_base[], int p0, int pn,
			PointPlotter plotter, double res) {
		double k1, k2;

		// Set up points for first curve segment.
		int p1 = p0;
		++p1;
		int p2 = p1;
		++p2;
		int p3 = p2;
		++p3;

		// Draw each curve segment.
		for (; p2 != pn; ++p0, ++p1, ++p2, ++p3) {
			// p1 and p2 equal; single point.
			if (x(f0_base, p1) == x(f0_base, p2)) {
				continue;
			}
			// Both end points repeated; straight line.
			if (x(f0_base, p0) == x(f0_base, p1)
					&& x(f0_base, p2) == x(f0_base, p3)) {
				k1 = k2 = (y(f0_base, p2) - y(f0_base, p1))
						/ (x(f0_base, p2) - x(f0_base, p1));
			}
			// p0 and p1 equal; use f''(x1) = 0.
			else if (x(f0_base, p0) == x(f0_base, p1)) {
				k2 = (y(f0_base, p3) - y(f0_base, p1))
						/ (x(f0_base, p3) - x(f0_base, p1));
				k1 = (3 * (y(f0_base, p2) - y(f0_base, p1))
						/ (x(f0_base, p2) - x(f0_base, p1)) - k2) / 2;
			}
			// p2 and p3 equal; use f''(x2) = 0.
			else if (x(f0_base, p2) == x(f0_base, p3)) {
				k1 = (y(f0_base, p2) - y(f0_base, p0))
						/ (x(f0_base, p2) - x(f0_base, p0));
				k2 = (3 * (y(f0_base, p2) - y(f0_base, p1))
						/ (x(f0_base, p2) - x(f0_base, p1)) - k1) / 2;
			}
			// Normal curve.
			else {
				k1 = (y(f0_base, p2) - y(f0_base, p0))
						/ (x(f0_base, p2) - x(f0_base, p0));
				k2 = (y(f0_base, p3) - y(f0_base, p1))
						/ (x(f0_base, p3) - x(f0_base, p1));
			}

			if (SPLINE_BRUTE_FORCE) {
				interpolate_brute_force(x(f0_base, p1), y(f0_base, p1), x(
						f0_base, p2), y(f0_base, p2), k1, k2, plotter, res);
			} else {
				interpolate_forward_difference(x(f0_base, p1), y(f0_base, p1),
						x(f0_base, p2), y(f0_base, p2), k1, k2, plotter, res);
			}
		}
	}

	// ----------------------------------------------------------------------------
	// END Spline functions.
	// ----------------------------------------------------------------------------

}
