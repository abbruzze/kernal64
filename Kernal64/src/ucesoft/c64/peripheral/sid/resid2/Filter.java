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
 * ---------------------------------------------------------------------------
 * Filter distortion code written by Antti S. Lankila 2007 - 2008.
 * 
 * @author Ken Händel
 *
 */
package ucesoft.c64.peripheral.sid.resid2;

/** 
 * SID filter base class
 * 
 * @author Ken Händel
 * @author Dag Lem
 * @author Antti Lankila
 */
public abstract class Filter {

	/**
	 * Filter enabled.
	 */
	private boolean enabled = true;

	/**
	 * Filter cutoff frequency.
	 */
	protected int fc;

	/**
	 * Filter resonance.
	 */
	protected int res;

	/**
	 * Selects which inputs to route through filter.
	 */
	private byte filt;
	
	/**
	 * Routing to filter or outside filter
	 */
	protected boolean filt1, filt2, filt3, filtE;

	/**
	 * Switch voice 3 off.
	 */
	protected boolean voice3off;

	/**
	 * Highpass, bandpass, and lowpass filter modes.
	 */
	protected boolean hp, bp, lp;

	/**
	 * Current volume.
	 */
	protected int vol;

	/**
	 * Current clock frequency.
	 */
	protected double clockFrequency;

	/**
	 * SID clocking - 1 cycle
	 * 
	 * @param v1 voice 1 in
	 * @param v2 voice 2 in
	 * @param v3 voice 3 in
	 * @return filtered output
	 */
	protected abstract int clock(int v1, int v2, int v3);

	/**
	 * Enable filter.
	 * 
	 * @param enable Enable/Disable the filter.
	 */
	public void enable(final boolean enable) {
		enabled = enable;
		if (enabled) {
			writeRES_FILT(filt);
		} else {
			filt1 = filt2 = filt3 = filtE = false;
			lp = bp = hp = false;
		}
	}

	protected void setClockFrequency(final double clock) {
		clockFrequency = clock;
		updatedCenterFrequency();
	}

	/**
	 * SID reset.
	 */
	protected final void reset() {
		writeFC_LO((byte) 0);
		writeFC_HI((byte) 0);
		writeMODE_VOL((byte) 0);
		writeRES_FILT((byte) 0);
	}

	/**
	 * Register function.
	 * 
	 * @param fc_lo
	 */
	protected final void writeFC_LO(final byte fc_lo) {
		fc = fc & 0x7f8 | fc_lo & 0x007;
		updatedCenterFrequency();
	}

	/**
	 * Register function.
	 * 
	 * @param fc_hi
	 */
	protected final void writeFC_HI(final byte fc_hi) {
		fc = fc_hi << 3 & 0x7f8 | fc & 0x007;
		updatedCenterFrequency();
	}

	/**
	 * Register function.
	 * 
	 * @param res_filt
	 */
	protected final void writeRES_FILT(final byte res_filt) {
		filt = res_filt;

		res = res_filt >> 4 & 0x0f;
		updatedResonance();

		if (enabled) {
			filt1 = (filt & 1) != 0;
			filt2 = (filt & 2) != 0;
			filt3 = (filt & 4) != 0;
			filtE = (filt & 8) != 0;
		}
		
		updatedMixing();
	}

	/**
	 * Register function.
	 * 
	 * @param mode_vol
	 */
	protected final void writeMODE_VOL(final byte mode_vol) {
		vol = mode_vol & 0xf;
		lp = (mode_vol & 0x10) != 0;
		bp = (mode_vol & 0x20) != 0;
		hp = (mode_vol & 0x40) != 0;
		voice3off = (mode_vol & 0x80) != 0;
		
		updatedMixing();
	}

	/**
	 * Set filter cutoff frequency.
	 */
	protected abstract void updatedCenterFrequency();

	/**
	 * Set filter resonance.
	 */
	protected abstract void updatedResonance();
	
	/**
	 * Mixing configuration modified (offsets change)
	 */
	protected abstract void updatedMixing();
	
	protected abstract void input(int input);
}
