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
package ucesoft.c64.peripheral.sid.resid2;


/**
 * A 24 bit accumulator is the basis for waveform generation. FREQ is added to the lower 16 bits of the accumulator each cycle. The accumulator is set to zero when TEST is set, and starts counting
 * when TEST is cleared. The noise waveform is taken from intermediate bits of a 23 bit shift register. This register is clocked by bit 19 of the accumulator.
 * 
 * Java port of the reSID 1.0 waveformgenerator by Dag Lem.
 * 
 * @author Ken Handel
 * @author Dag Lem
 * @author Antti Lankila
 */
public final class WaveformGenerator {
	private final short[][] model_wave = new short[8][4096];
	private final short[] dac = new short[4096];
	
	/** Current and previous accumulator value. */
	protected int accumulator;
	
	// Tell whether the accumulator MSB was set high on this cycle.
	private boolean msb_rising;

	// Fout  = (Fn*Fclk/16777216)Hz
	// reg16 freq;
	protected int freq;
	// PWout = (PWn/40.95)%
	private int pw;

	private int shift_register;

	// Remaining time to fully reset shift register.
	private int shift_register_reset;
	// Emulation of pipeline causing bit 19 to clock the shift register.
	private int shift_pipeline;

	private int ring_msb_mask;
	private int no_noise;
	private int noise_output;
	private int no_noise_or_noise_output;
	private int no_pulse;
	private int pulse_output;
	  
	/**
	 * The control register right-shifted 4 bits; used for output function table lookup.
	 */
	private int waveform;

	/**
	 * The control register bits. Gate is handled by EnvelopeGenerator.
	 */
	protected boolean test;
	protected boolean sync;

	private int floating_output_ttl;
	
	private short[] wave;
	
	private int waveform_output;

	protected void setWaveformModels(short[][] models) {
		for (int i = 0; i < 8; i ++) {
			System.arraycopy(models[i], 0, model_wave[i], 0, 4096);
		}
	}

	/**
	 * Set nonlinearity parameter for imperfect analog DAC emulation.
	 * 1.0 means perfect 8580-like linearity, values between 0.95 - 0.97
	 * are probably realistic 6581 nonlinearity values.
	 * 
	 * @param chipModel The {@link ChipModel} to use.
	 */
	protected void setChipModel(final ChipModel chipModel) {
		final double dacBits[] = new double[12];
		SID.kinkedDac(dacBits, chipModel == ChipModel.MOS6581 ? 2.20 : 2.00, chipModel == ChipModel.MOS8580);
		
		for (int i = 0; i < 4096; i++) {
			double dacValue = 0;
			for (int j = 0; j < 12; j ++) {
				if ((i & (1 << j)) != 0) {
					dacValue += dacBits[j];
				}
			}
			dac[i] = (short) (dacValue + 0.5);
		}
		int offset = dac[chipModel == ChipModel.MOS6581 ? 0x380 : 0x800];
		for (int i = 0; i < 4096; i ++) {
			dac[i] -= offset;
		}
	}

	/**
	 * SID clocking - 1 cycle.
	 */
	protected void clock() {
		if (test) {
			if (shift_register_reset != 0 && -- shift_register_reset == 0) {
				reset_shift_register();
			}

			// The test bit sets pulse high.
			pulse_output = 0xfff;
		} else {
			// Calculate new accumulator value;
			int accumulator_next = (accumulator + freq) & 0xffffff;
			int accumulator_bits_set = ~accumulator & accumulator_next;
			accumulator = accumulator_next;

			// Check whether the MSB is set high. This is used for synchronization.
			msb_rising = (accumulator_bits_set & 0x800000) != 0;

			// Shift noise register once for each time accumulator bit 19 is set high.
			// The shift is delayed 2 cycles.
			if ((accumulator_bits_set & 0x080000) != 0) {
				// Pipeline: Detect rising bit, shift phase 1, shift phase 2.
				shift_pipeline = 2;
			}
			else if (shift_pipeline != 0 && -- shift_pipeline == 0) {
				clock_shift_register();
			}
		}
	}

	/**
	 * Synchronize oscillators. This must be done after all the oscillators have been clock()'ed,
	 * so that they are in the same state.
	 *
	 * @param syncDest The oscillator I am syncing
	 * @param syncSource The oscillator syncing me.
	 */
	protected final void synchronize(final WaveformGenerator syncDest, final WaveformGenerator syncSource) {
		// A special case occurs when a sync source is synced itself on the same
		// cycle as when its MSB is set high. In this case the destination will
		// not be synced. This has been verified by sampling OSC3.
		if (msb_rising && syncDest.sync && !(sync && syncSource.msb_rising)) {
			syncDest.accumulator = 0;
		}
	}

	private void clock_shift_register() {
		// bit0 = (bit22 | test) ^ bit17
		int bit0 = ((shift_register >> 22) ^ (shift_register >> 17)) & 0x1;
		shift_register = ((shift_register << 1) | bit0) & 0x7fffff;

		// New noise waveform output.
		set_noise_output();
	}

	private void write_shift_register() {
	  // Write changes to the shift register output caused by combined waveforms
	  // back into the shift register.
	  // A bit once set to zero cannot be changed, hence the and'ing.
	  // FIXME: Write test program to check the effect of 1 bits and whether
	  // neighboring bits are affected.

	  shift_register &=
	    ~((1<<20)|(1<<18)|(1<<14)|(1<<11)|(1<<9)|(1<<5)|(1<<2)|(1<<0)) |
	    ((waveform_output & 0x800) << 9) |  // Bit 11 -> bit 20
	    ((waveform_output & 0x400) << 8) |  // Bit 10 -> bit 18
	    ((waveform_output & 0x200) << 5) |  // Bit  9 -> bit 14
	    ((waveform_output & 0x100) << 3) |  // Bit  8 -> bit 11
	    ((waveform_output & 0x080) << 2) |  // Bit  7 -> bit  9
	    ((waveform_output & 0x040) >> 1) |  // Bit  6 -> bit  5
	    ((waveform_output & 0x020) >> 3) |  // Bit  5 -> bit  2
	    ((waveform_output & 0x010) >> 4);   // Bit  4 -> bit  0

	  noise_output &= waveform_output;
	  no_noise_or_noise_output = no_noise | noise_output;
	}
	
	private void reset_shift_register()
	{
	  shift_register = 0x7fffff;
	  shift_register_reset = 0;

	  // New noise waveform output.
	  set_noise_output();
	}
	
	private void set_noise_output() {
	  noise_output =
	    ((shift_register & 0x100000) >> 9) |
	    ((shift_register & 0x040000) >> 8) |
	    ((shift_register & 0x004000) >> 5) |
	    ((shift_register & 0x000800) >> 3) |
	    ((shift_register & 0x000200) >> 2) |
	    ((shift_register & 0x000020) << 1) |
	    ((shift_register & 0x000004) << 3) |
	    ((shift_register & 0x000001) << 4);

	  no_noise_or_noise_output = no_noise | noise_output;
	}

	/**
	 * 12-bit waveform output.
	 * 
	 * @param ringModulator The oscillator ring-modulating me.
	 * @return output from waveformgenerator
	 */
	public short output(final WaveformGenerator ringModulator) {
		// Set output value.
		if (waveform != 0) {
			// The bit masks no_pulse and no_noise are used to achieve branch-free
			// calculation of the output value.
			int ix = (accumulator ^ (ringModulator.accumulator & ring_msb_mask)) >> 12;
	    	waveform_output = wave[ix] & (no_pulse | pulse_output) & no_noise_or_noise_output;
	    	if (waveform > 0x8) {
	    		// Combined waveforms write to the shift register.
	    		write_shift_register();
	    	}
		}
		else {
			// Age floating DAC input.
			if (floating_output_ttl != 0 && -- floating_output_ttl == 0) {
				waveform_output = 0;
			}
		}

		// The pulse level is defined as (accumulator >> 12) >= pw ? 0xfff : 0x000.
		// The expression -((accumulator >> 12) >= pw) & 0xfff yields the same
		// results without any branching (and thus without any pipeline stalls).
		// NB! This expression relies on that the result of a boolean expression
		// is either 0 or 1, and furthermore requires two's complement integer.
		// A few more cycles may be saved by storing the pulse width left shifted
		// 12 bits, and dropping the and with 0xfff (this is valid since pulse is
		// used as a bit mask on 12 bit values), yielding the expression
		// -(accumulator >= pw24). However this only results in negligible savings.

		// The result of the pulse width compare is delayed one cycle.
		// Push next pulse level into pulse level pipeline.
		pulse_output = ((accumulator >> 12) >= pw) ? 0xfff : 0x000;

		// DAC imperfections are emulated by using waveform_output as an index
		// into a DAC lookup table. readOSC() uses waveform_output directly.
		return dac[waveform_output];
	}

	/**
	 * Constructor.
	 */
	protected WaveformGenerator() {}

	/**
	 * Register functions.
	 * 
	 * @param freq_lo low 8 bits of frequency
	 */
	protected void writeFREQ_LO(final byte freq_lo) {
		freq = freq & 0xff00 | freq_lo & 0xff;
	}

	/**
	 * Register functions.
	 * 
	 * @param freq_hi high 8 bits of frequency
	 */
	protected void writeFREQ_HI(final byte freq_hi) {
		freq = freq_hi << 8 & 0xff00 | freq & 0xff;
	}

	/**
	 * Register functions.
	 * 
	 * The original form was (acc >> 12) >= pw, where truth value is not affected by the contents of the low 12 bits. Therefore the lowest bits must be zero in the new formulation acc >= (pw << 12).
	 * 
	 * @param pw_lo low 8 bits of pulse width
	 */
	protected void writePW_LO(final byte pw_lo) {
		pw = pw & 0xf00 | pw_lo & 0x0ff;
	}

	/**
	 * Register functions.
	 * 
	 * @param pw_hi high 8 bits of pulse width
	 */
	protected void writePW_HI(final byte pw_hi) {
		pw = pw_hi << 8 & 0xf00 | pw & 0x0ff;
	}

	/**
	 * Register functions.
	 *
	 * @param control control register value
	 */
	protected void writeCONTROL_REG(final byte control) {
		int waveform_prev = waveform;
		boolean test_prev = test;
		waveform = (control >> 4) & 0x0f;
		test = (control & 0x08) != 0;
		sync = (control & 0x02) != 0;

	  // Set up waveform table.
	  wave = model_wave[waveform & 0x7];

	  // Substitution of accumulator MSB when sawtooth = 0, ring_mod = 1.
	  ring_msb_mask = ((~control >> 5) & (control >> 2) & 0x1) << 23;

	  // no_noise and no_pulse are used in set_waveform_output() as bitmasks to
	  // only let the noise or pulse influence the output when the noise or pulse
	  // waveforms are selected.
	  no_noise = (waveform & 0x8) != 0 ? 0x000 : 0xfff;
	  no_noise_or_noise_output = no_noise | noise_output;
	  no_pulse = (waveform & 0x4) != 0 ? 0x000 : 0xfff;

	  if (!test_prev && test) {
		  // Reset accumulator.
		  accumulator = 0;

		  // Flush shift pipeline.
		  shift_pipeline = 0;
		  
		  // Set reset time for shift register.
		  shift_register_reset = 0x8000;
	  }	
	  else if (test_prev && !test) {
		  // When the test bit is falling, the second phase of the shift is
		  // completed by enabling SRAM write.

		  // bit0 = (bit22 | test) ^ bit17 = 1 ^ bit17 = ~bit17
		  int bit0 = (~shift_register >> 17) & 0x1;
		  shift_register = ((shift_register << 1) | bit0) & 0x7fffff;
		  
		  // Set new noise waveform output.
		  set_noise_output();
	  }

	  if (waveform == 0 && waveform_prev != 0) {
		  // Change to floating DAC input.
		  // Reset fading time for floating DAC input.
		  floating_output_ttl = 0x4000;
	  }
	}

	/**
	 * Read OSC3 value (6581, not latched/delayed version)
	 *
	 * @return OSC3 value
	 */
	public byte readOSC() {
		return (byte) (waveform_output >> 4);
	}

	/**
	 * SID reset.
	 */
	protected void reset() {
		accumulator = 0;
		freq = 0;
		pw = 0;

		msb_rising = false;

		waveform = 0;
		test = false;
		sync = false;

		wave = model_wave[0];

		ring_msb_mask = 0;
		no_noise = 0xfff;
		no_pulse = 0xfff;
		pulse_output = 0xfff;
		
		reset_shift_register();
		shift_pipeline = 0;

		waveform_output = 0;
		floating_output_ttl = 0;
	}
}
