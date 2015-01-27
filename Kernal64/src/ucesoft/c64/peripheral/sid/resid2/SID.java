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

import  ucesoft.c64.peripheral.sid.resid2.resample.*;;

/**
 * MOS6581/MOS8580 emulation.
 * Based on reSID 0.16 by Dag Lem,
 * and then hacked on by Antti S. Lankila.
 * Ported to Java by Ken Handel.
 * 
 * @author Ken Handel
 * @author Dag Lem
 * @author Antti Lankila
 */
public final class SID {
	/**
	 * Bus value stays alive for some time after each operation.
	 */
	private static final int BUS_TTL = 0x9000;

	/** SID voices */
	public final Voice[] voice = new Voice[] { new Voice(), new Voice(), new Voice() };

	/** Currently active filter */
	private Filter filter;

	/** Filter used, if model is set to 6581 */
	private final Filter6581 filter6581 = new Filter6581();

	/** Filter used, if model is set to 8580 */
	private final Filter8580 filter8580 = new Filter8580();

	/**
	 * External filter that provides high-pass and low-pass filtering
	 * to adjust sound tone slightly.
	 */
	private final ExternalFilter externalFilter = new ExternalFilter();

	/** Paddle X register support */
	private final Potentiometer potX = new Potentiometer();

	/** Paddle Y register support */
	private final Potentiometer potY = new Potentiometer();

	/** Last written value */
	private byte busValue;

	/** Time to live for the last written value */
	private int busValueTtl;

	/**
	 * Currently active chip model.
	 */
	private ChipModel model;

	/**
	 * Time until synchronize() must be run.
	 */
	private int nextVoiceSync;

	/**
	 * Resampler used by audio generation code.
	 */
	private Resampler resampler;

	/** Delayed MOS8580 write register */
	private int delayedOffset;

	/** Delayed MOS8580 write value */
	private byte delayedValue;

	private final boolean[] muted = new boolean[3];

	/**
	 * Estimate DAC nonlinearity. The SID contains R-2R ladder, and some likely errors
	 * in the resistor lengths which result in errors depending on the bits chosen.
	 * <P>
	 * This model was derived by Dag Lem, and is port of the upcoming reSID version.
	 * In average, it shows a value higher than the target by a value that depends
	 * on the _2R_div_R parameter. It differs from the version written by Antti Lankila
	 * chiefly in the emulation of the lacking termination of the 2R ladder, which
	 * destroys the output with respect to the low bits of the DAC.
	 * <P>
	 * Returns the analog value as modeled from the R-2R network.
	 *
	 * @param dac digital value to convert to analog
	 * @param _2R_div_R nonlinearity parameter, 1.0 for perfect linearity.
	 * @param term is the dac terminated by a 2R resistor? (6581 DACs are not)
	 */
	public static void kinkedDac(final double[] dac, final double _2R_div_R, final boolean term) {
		final double INFINITY = 1e6;

		// Calculate voltage contribution by each individual bit in the R-2R ladder.
		for (int set_bit = 0; set_bit < dac.length; set_bit++) {
			int bit;

		    double Vn = 1;          // Normalized bit voltage.
		    double R = 1;           // Normalized R
		    double _2R = _2R_div_R*R;  // 2R
		    double Rn = term ?         // Rn = 2R for correct termination,
		      _2R : INFINITY;         // INFINITY for missing termination.

		    // Calculate DAC "tail" resistance by repeated parallel substitution.
		    for (bit = 0; bit < set_bit; bit++) {
		    	if (Rn == INFINITY) {
		    		Rn = R + _2R;
		    	} else {
		    		Rn = R + _2R*Rn/(_2R + Rn); // R + 2R || Rn
		    	}
		    }
		    
		    // Source transformation for bit voltage.
		    if (Rn == INFINITY) {
		      Rn = _2R;
		    }
		    else {
		      Rn = _2R*Rn/(_2R + Rn);  // 2R || Rn
		      Vn = Vn*Rn/_2R;
		    }
		    
		    // Calculate DAC output voltage by repeated source transformation from
		    // the "tail".
		    
		    for (++bit; bit < dac.length; bit++) {
		        Rn += R;
		        double I = Vn/Rn;
		        Rn = _2R*Rn/(_2R + Rn);  // 2R || Rn
		        Vn = Rn*I;
		    }
		    
		    dac[set_bit] = Vn;
		}

		/* Normalize to integerish behavior */
		double Vsum = 0;
		for (int i = 0; i < dac.length; i ++) {
			Vsum += dac[i];
		}
		Vsum /= 1 << dac.length;
		for (int i = 0; i < dac.length; i ++) {
			dac[i] /= Vsum;
		}
	}

	/**
	 * Constructor.
	 */
	public SID() {
		reset();
		setChipModel(ChipModel.MOS8580);
	}

	/**
	 * Set chip model.
	 * 
	 * @param model chip model to use
	 */
	public void setChipModel(final ChipModel model) {
		this.model = model;

		if (model == ChipModel.MOS6581) {
			filter = filter6581;
		} else if (model == ChipModel.MOS8580) {
			filter = filter8580;
		} else {
			throw new RuntimeException("Don't know how to handle chip type " + model);
		}

		/* calculate waveform-related tables, feed them to the generator */
		final short[][] tables = WaveformCalculator.buildTable(model);

		/* update voice offsets */
		for (int i = 0; i < 3; i++) {
			voice[i].envelope.setChipModel(model);
			voice[i].wave.setChipModel(model);
			voice[i].wave.setWaveformModels(tables);
		}
	}

	public ChipModel getChipModel() {
		return model;
	}

	/**
	 * SID reset.
	 */
	public void reset() {
		for (int i = 0; i < 3; i++) {
			voice[i].reset();
		}
		filter6581.reset();
		filter8580.reset();
		externalFilter.reset();
		if (resampler != null) {
			resampler.reset();
		}

		busValue = 0;
		busValueTtl = 0;
		delayedOffset = -1;
		voiceSync(false);
	}

	/**
	 * 16-bit input (EXT IN). Write 16-bit sample to audio input. NB! The caller
	 * is responsible for keeping the value within 16 bits. Note that to mix in
	 * an external audio signal, the signal should be resampled to 1MHz first to
	 * avoid sampling noise.
	 * 
	 * @param value input level to set
	 */
	public void input(final int value) {
		filter6581.input(value);
		filter8580.input(value);
	}

	/**
	 * Read registers.
	 * <P>
	 * Reading a write only register returns the last byte written to any SID register. The individual bits in this value start to fade down towards zero after a few cycles. All bits reach zero within
	 * approximately $2000 - $4000 cycles. It has been claimed that this fading happens in an orderly fashion, however sampling of write only registers reveals that this is not the case. NB! This is
	 * not correctly modeled. The actual use of write only registers has largely been made in the belief that all SID registers are readable. To support this belief the read would have to be done
	 * immediately after a write to the same register (remember that an intermediate write to another register would yield that value instead). With this in mind we return the last value written to
	 * any SID register for $2000 cycles without modeling the bit fading.
	 * 
	 * @param offset SID register to read
	 * @return value read from chip
	 */
	public byte read(final int offset) {
		final byte value;
		
		switch (offset) {
		case 0x19:
			value = potX.readPOT();
			busValueTtl = BUS_TTL;
			break;
		case 0x1a:
			value = potY.readPOT();
			busValueTtl = BUS_TTL;
			break;
		case 0x1b:
			value = voice[2].wave.readOSC();
			break;
		case 0x1c:
			value = voice[2].envelope.readENV();
			busValueTtl = BUS_TTL;
			break;
		default:
			value = busValue;
			busValueTtl /= 2;
			break;
		}

		busValue = value;		
		return value;
	}

	/**
	 * Write registers.
	 * 
	 * @param offset chip register to write
	 * @param value value to write
	 */
	public void write(final int offset, final byte value) {
		busValue = value;
		busValueTtl = BUS_TTL;

		if (model == ChipModel.MOS8580) {
			delayedOffset = offset;
			delayedValue = value;
		} else {
			writeImmediate(offset, value);
		}
	}
	
	private void writeImmediate(final int offset, final byte value) {
		switch (offset) {
		case 0x00:
			voice[0].wave.writeFREQ_LO(value);
			break;
		case 0x01:
			voice[0].wave.writeFREQ_HI(value);
			break;
		case 0x02:
			voice[0].wave.writePW_LO(value);
			break;
		case 0x03:
			voice[0].wave.writePW_HI(value);
			break;
		case 0x04:
			voice[0].writeCONTROL_REG(muted[0] ? 0 : value);
			break;
		case 0x05:
			voice[0].envelope.writeATTACK_DECAY(value);
			break;
		case 0x06:
			voice[0].envelope.writeSUSTAIN_RELEASE(value);
			break;
		case 0x07:
			voice[1].wave.writeFREQ_LO(value);
			break;
		case 0x08:
			voice[1].wave.writeFREQ_HI(value);
			break;
		case 0x09:
			voice[1].wave.writePW_LO(value);
			break;
		case 0x0a:
			voice[1].wave.writePW_HI(value);
			break;
		case 0x0b:
			voice[1].writeCONTROL_REG(muted[1] ? 0 : value);
			break;
		case 0x0c:
			voice[1].envelope.writeATTACK_DECAY(value);
			break;
		case 0x0d:
			voice[1].envelope.writeSUSTAIN_RELEASE(value);
			break;
		case 0x0e:
			voice[2].wave.writeFREQ_LO(value);
			break;
		case 0x0f:
			voice[2].wave.writeFREQ_HI(value);
			break;
		case 0x10:
			voice[2].wave.writePW_LO(value);
			break;
		case 0x11:
			voice[2].wave.writePW_HI(value);
			break;
		case 0x12:
			voice[2].writeCONTROL_REG(muted[2] ? 0 : value);
			break;
		case 0x13:
			voice[2].envelope.writeATTACK_DECAY(value);
			break;
		case 0x14:
			voice[2].envelope.writeSUSTAIN_RELEASE(value);
			break;
		case 0x15:
			filter6581.writeFC_LO(value);
			filter8580.writeFC_LO(value);
			break;
		case 0x16:
			filter6581.writeFC_HI(value);
			filter8580.writeFC_HI(value);
			break;
		case 0x17:
			filter6581.writeRES_FILT(value);
			filter8580.writeRES_FILT(value);
			break;
		case 0x18:
			filter6581.writeMODE_VOL(value);
			filter8580.writeMODE_VOL(value);
			break;
		default:
			break;
		}
		
		/* Update voicesync just in case. */
		voiceSync(false);
	}

	/**
	 * SID voice muting.
	 * 
	 * @param channel channe to modify
	 * @param enable is muted?
	 */
	public void mute(final int channel, final boolean enable) {
		muted[channel] = enable;
	}

	/**
	 * Setting of SID sampling parameters.
	 * <P>
	 * Use a clock freqency of 985248Hz for PAL C64, 1022730Hz for NTSC C64. The default end of passband frequency is pass_freq = 0.9*sample_freq/2 for sample frequencies up to ~ 44.1kHz, and 20kHz
	 * for higher sample frequencies.
	 * <P>
	 * For resampling, the ratio between the clock frequency and the sample frequency is limited as follows: 125*clock_freq/sample_freq < 16384 E.g. provided a clock frequency of ~ 1MHz, the sample
	 * frequency can not be set lower than ~ 8kHz. A lower sample frequency would make the resampling code overfill its 16k sample ring buffer.
	 * <P>
	 * The end of passband frequency is also limited: pass_freq <= 0.9*sample_freq/2
	 * <P>
	 * E.g. for a 44.1kHz sampling rate the end of passband frequency is limited to slightly below 20kHz. This constraint ensures that the FIR table is not overfilled.
	 * 
	 * @param clockFrequency System clock frequency at Hz
	 * @param method sampling method to use
	 * @param samplingFrequency Desired output sampling rate
	 */
	public void setSamplingParameters(final double clockFrequency, final SamplingMethod method, final double samplingFrequency, final double highestAccurateFrequency) {
		filter6581.setClockFrequency(clockFrequency);
		filter8580.setClockFrequency(clockFrequency);
		externalFilter.setClockFrequency(clockFrequency);

		switch (method) {
		case DECIMATE:
			resampler = new ZeroOrderResampler(clockFrequency, samplingFrequency);
			break;
		case RESAMPLE:
			resampler = new TwoPassSincResampler(clockFrequency, samplingFrequency, highestAccurateFrequency);
			break;
		default:
			throw new RuntimeException("Unknown samplingmethod: " + method);
		}
	}

	private void ageBusValue(final int n) {
		if (busValueTtl != 0) {
			busValueTtl -= n;
			if (busValueTtl <= 0) {
				busValue = 0;
				busValueTtl = 0;
			}
		}
	}

	/**
	 * Get output sample
	 * 
	 * @return the output sample
	 */
	private int output() {
		return externalFilter.clock(filter.clock(voice[0].output(voice[2].wave), voice[1].output(voice[0].wave), voice[2].output(voice[1].wave)));
	}

	/**
	 * Clock SID forward using chosen output sampling algorithm.
	 * 
	 * @param cycles c64 clocks to clock
	 * @param buf audio output buffer
	 * @param pos where to begin audio writing
	 * @return
	 */
	public final int clock(int cycles, final int buf[], final int pos) {
		ageBusValue(cycles);
		int s = 0;
		
		while (cycles != 0) {
			int delta_t = Math.min(nextVoiceSync, cycles);
			if (delta_t > 0) {
				if (delayedOffset != -1) {
					delta_t = 1;
				}
				
				for (int i = 0; i < delta_t; i ++) {
					if (resampler.input(output())) {
						buf[pos + s] = resampler.output();
						s ++;
					}

					/* clock waveform generators */
					voice[0].wave.clock();
					voice[1].wave.clock();
					voice[2].wave.clock();
				
					/* clock envelope generators */
					voice[0].envelope.clock();
					voice[1].envelope.clock();
					voice[2].envelope.clock();
				}
				
				if (delayedOffset != -1) {
					writeImmediate(delayedOffset, delayedValue);
					delayedOffset = -1;
				}
		
				cycles -= delta_t;
				nextVoiceSync -= delta_t;
			}
			
			if (nextVoiceSync == 0) {
				voiceSync(true);
			}
		}

		return s;
	}

	/**
	 * Clock SID forward with no audio production.
	 * <p>
	 * <b>Warning:</b>
	 * You can't mix this method of clocking with the audio-producing
	 * clock() because components that don't affect OSC3/ENV3 are not
	 * emulated.
	 * 
	 * @param cycles c64 clocks to clock.
	 */
	public void clockSilent(int cycles) {
		ageBusValue(cycles);

		while (cycles != 0) {
			int delta_t = Math.min(nextVoiceSync, cycles);
			if (delta_t > 0) {
				if (delayedOffset != -1) {
					delta_t = 1;
				}
				
				for (int i = 0; i < delta_t; i ++) {
					/* clock waveform generators (can affect OSC3) */
					voice[0].wave.clock();
					voice[1].wave.clock();
					voice[2].wave.clock();

					/* clock ENV3 only */
					voice[2].envelope.clock();
				}

				if (delayedOffset != -1) {
					writeImmediate(delayedOffset, delayedValue);
					delayedOffset = -1;
				}

				cycles -= delta_t;
				nextVoiceSync -= delta_t;
			}
			
			if (nextVoiceSync == 0) {
				voiceSync(true);
			}
		}
	}

	/**
	 * Return the numebr of cycles according to current parameters
	 * that it takes to reach sync.
	 */
	private void voiceSync(boolean sync) {
		if (sync) {
			/* Synchronize the 3 waveform generators. */
			for (int i = 0; i < 3 ; i ++) {
				voice[i].wave.synchronize(voice[(i+1) % 3].wave, voice[(i+2) % 3].wave);
			}
		}
		
		/* Calculate the time to next voice sync */
		nextVoiceSync = Integer.MAX_VALUE;
		for (int i = 0; i < 3 ; i ++) {
			int accumulator = voice[i].wave.accumulator;
			int freq = voice[i].wave.freq;

			if (voice[i].wave.test || freq == 0 || !voice[(i+1) % 3].wave.sync) {
				continue;
			}
			
			int thisVoiceSync = ((0x7fffff - accumulator) & 0xffffff) / freq + 1;
			if (thisVoiceSync < nextVoiceSync) {
				nextVoiceSync = thisVoiceSync;
			}
		}
	}

	/**
	 * Get chip's 6581 filter.
	 * 
	 * @return filter
	 */
	public Filter6581 getFilter6581() {
		return filter6581;
	}

	/**
	 * Get chip's 8580 filter.
	 * 
	 * @return filter
	 */
	public Filter8580 getFilter8580() {
		return filter8580;
	}
}
