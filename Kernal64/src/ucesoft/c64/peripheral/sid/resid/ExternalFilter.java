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
 * 
 * The audio output stage in a Commodore 64 consists of two STC networks, a
 * low-pass filter with 3-dB frequency 16kHz followed by a high-pass filter with
 * 3-dB frequency 16Hz (the latter provided an audio equipment input impedance
 * of 1kOhm).
 * <P>
 * The STC networks are connected with a BJT supposedly meant to act as a unity
 * gain buffer, which is not really how it works. A more elaborate model would
 * include the BJT, however DC circuit analysis yields BJT base-emitter and
 * emitter-base impedances sufficiently low to produce additional low-pass and
 * high-pass 3dB-frequencies in the order of hundreds of kHz. This calls for a
 * sampling frequency of several MHz, which is far too high for practical use.
 * 
 * @author Ken Handel
 * 
 */
public class ExternalFilter {

	/**
	 * Filter enabled.
	 */
	protected boolean enabled;

	/**
	 * Maximum mixer DC offset.
	 */
	protected int /* sound_sample */mixer_DC;

	/**
	 * State of filters. lowpass
	 */
	protected int /* sound_sample */Vlp;

	/**
	 * State of filters. highpass
	 */
	protected int /* sound_sample */Vhp;

	/**
	 * State of filters.
	 */
	protected int /* sound_sample */Vo;

	/**
	 * Cutoff frequencies.
	 */
	protected int /* sound_sample */w0lp;

	/**
	 * Cutoff frequencies.
	 */
	protected int /* sound_sample */w0hp;

	// ----------------------------------------------------------------------------
	// Inline functions.
	// The following functions are defined inline because they are called every
	// time a sample is calculated.
	// ----------------------------------------------------------------------------

	/**
	 * SID clocking - 1 cycle.
	 * 
	 * @param Vi
	 */
	public void clock(int /* sound_sample */Vi) {
		// This is handy for testing.
		if (!enabled) {
			// Remove maximum DC level since there is no filter to do it.
			Vlp = Vhp = 0;
			Vo = Vi - mixer_DC;
			return;
		}

		// delta_t is converted to seconds given a 1MHz clock by dividing
		// with 1 000 000.

		// Calculate filter outputs.
		// Vo = Vlp - Vhp;
		// Vlp = Vlp + w0lp*(Vi - Vlp)*delta_t;
		// Vhp = Vhp + w0hp*(Vlp - Vhp)*delta_t;

		int /* sound_sample */dVlp = (w0lp >> 8) * (Vi - Vlp) >> 12;
		int /* sound_sample */dVhp = w0hp * (Vlp - Vhp) >> 20;
		Vo = Vlp - Vhp;
		Vlp += dVlp;
		Vhp += dVhp;
	}

	/**
	 * SID clocking - delta_t cycles.
	 * 
	 * @param delta_t
	 * @param Vi
	 */
	public void clock(int /* cycle_count */delta_t, int /* sound_sample */Vi) {
		// This is handy for testing.
		if (!enabled) {
			// Remove maximum DC level since there is no filter to do it.
			Vlp = Vhp = 0;
			Vo = Vi - mixer_DC;
			return;
		}

		// Maximum delta cycles for the external filter to work satisfactorily
		// is approximately 8.
		int /* cycle_count */delta_t_flt = 8;

		while (delta_t != 0) {
			if (delta_t < delta_t_flt) {
				delta_t_flt = delta_t;
			}

			// delta_t is converted to seconds given a 1MHz clock by dividing
			// with 1 000 000.

			// Calculate filter outputs.
			// Vo = Vlp - Vhp;
			// Vlp = Vlp + w0lp*(Vi - Vlp)*delta_t;
			// Vhp = Vhp + w0hp*(Vlp - Vhp)*delta_t;

			int /* sound_sample */dVlp = (w0lp * delta_t_flt >> 8)
					* (Vi - Vlp) >> 12;
			int /* sound_sample */dVhp = w0hp * delta_t_flt * (Vlp - Vhp) >> 20;
			Vo = Vlp - Vhp;
			Vlp += dVlp;
			Vhp += dVhp;

			delta_t -= delta_t_flt;
		}
	}

	/**
	 * Audio output (20 bits).
	 * 
	 * Audio output (19.5 bits).
	 * 
	 * @return Vo
	 */
	public int /* sound_sample */output() {
		return Vo;
	}

	// ----------------------------------------------------------------------------
	// END Inline functions.
	// ----------------------------------------------------------------------------

	/**
	 * Constructor.
	 */
	public ExternalFilter() {
		reset();
		enable_filter(true);
		set_sampling_parameter(15915.6);
		set_chip_model(ISIDDefs.chip_model.MOS6581);
	}

	/**
	 * Enable filter.
	 * 
	 * @param enable
	 * enable filter
	 */
	public void enable_filter(boolean enable) {
		enabled = enable;
	}

	/**
	 * Setup of the external filter sampling parameters.
	 * 
	 * @param pass_freq
	 */
	public void set_sampling_parameter(double pass_freq) {
		final double pi = 3.1415926535897932385;

		// Low-pass: R = 10kOhm, C = 1000pF; w0l = 1/RC = 1/(1e4*1e-9) = 100000
		// High-pass: R = 1kOhm, C = 10uF; w0h = 1/RC = 1/(1e3*1e-5) = 100
		// Multiply with 1.048576 to facilitate division by 1 000 000 by right-
		// shifting 20 times (2 ^ 20 = 1048576).

		w0hp = 105;
		w0lp = (int /* sound_sample */) (pass_freq * (2.0 * pi * 1.048576));
		if (w0lp > 104858)
			w0lp = 104858;
	}

	/**
	 * Set chip model.
	 * 
	 * @param model
	 * chip model
	 */
	public void set_chip_model(ISIDDefs.chip_model model) {
		if (model == ISIDDefs.chip_model.MOS6581) {
			// Maximum mixer DC output level; to be removed if the external
			// filter is turned off: ((wave DC + voice DC) * voices + mixer DC)
			// * volume
			// See Voice.java and Filter.java for an explanation of the values.
			mixer_DC = ((((0x800 - 0x380) + 0x800) * 0xff * 3 - 0xfff * 0xff / 18) >> 7) * 0x0f;
		} else {
			// No DC offsets in the MOS8580.
			mixer_DC = 0;
		}
	}

	/**
	 * SID reset.
	 */
	public void reset() {
		// State of filter.
		Vlp = 0;
		Vhp = 0;
		Vo = 0;
	}
}
