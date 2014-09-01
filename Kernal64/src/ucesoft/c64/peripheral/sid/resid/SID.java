
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
 * @author Ken Händel
 *
 */
package ucesoft.c64.peripheral.sid.resid;

public class SID {

	/**
	 * This is a patch against ReSID
	 *         engine in libsidplay2-2.1.1 to make its sound closer to 6581R4
	 *         chip.<BR>
	 * 
	 * http://bel.fi/~alankila/c64-sw/
	 * 
	 * <P>
	 * <B>Good simulation of strong lead and bass distortion.</B> Tel's leads,
	 * Jeff's bass, Mechanicus's extreme distortion: it all seems to reproduce
	 * quite nicely.
	 * 
	 * <P>
	 * <B>Good simulation of advanced effects peculiar to 6581</B>, especially
	 * on Jeff's songs that target 6581R4.
	 * 
	 * <P>
	 * <B>Parameters are now in sidplay2.ini file.</B> No recompile is
	 * necessary when testing alternative parameters.
	 * 
	 * <P>
	 * <B>Bugs in some songs, especially related to lowpass filter output.</B>
	 * The middle part of Spaceman Salutes Commodore and intro for Snake Disco
	 * are incorrectly rendered. (ReSID's linear filter is totally wrong with
	 * them too.) Vendetta probably gets too much edge in the bass, too. It
	 * seems that some kind of additional low-pass filtering needs to happen
	 * during so far unknown filter conditions.
	 * <P>
	 * <UL>
	 * <LI> 8.12.2007 Separate parameters for band-pass (high) and low-pass
	 * (low) distortion. This doubles the number of tunables, but seems like a
	 * necessary step to open experimentation on this front.
	 * <LI> 6.12.2007 New distortion algorithm. After I scratched the whole
	 * model of substracting filter states from each other. The new model simply
	 * calculates distortion by mixing in the filter state terms and input in
	 * linear combination. The following rules seem to apply:
	 * <OL>
	 * <LI> Vlp term must be about 0.9. More than that, and Jeff's bass sounds
	 * lose their edge. Too little, and pretty much everything distorts wrong.
	 * <LI> Vbp term be something negative like -0.5 or -1 or Vendetta and
	 * Meanwhile The Planet doesn't work.
	 * <LI> Vhp must be small or the distortion loses its "edge".
	 * </OL>
	 * 
	 * The new model has one important property above the previous one: it only
	 * distorts the other half-wave, the same which can be seen distorted in SID
	 * samplings, too. Therefore it makes for more complicated distorted sounds,
	 * and better replicates the effects in f.e. Mechanicus.
	 * <LI> 5.12.2007 Nearly doubled the impact of the bp-hp term. This improves
	 * Elite and Shades quite a bit, without seeming to negatively impact other
	 * songs.
	 * <LI> 4.12.2007 Made tunables out of the rest of the distortion
	 * parameters, save from one that I just added: low-pass filtering for my
	 * crude w0_eff term. This seems to reduce the artifacts on Robocop 3, and
	 * generally doesn't seem to harm the songs.
	 * <LI> 3.12.2007 A new distortion algorithm. A lot of time has passed. I
	 * made some improvements that force more distortion out of the model. I
	 * feel that this reinstates some of my LEADDIST effects and should be
	 * closer to correct model. I'm reasonably happy about how it turned out.
	 * This is definitely the best all-around emulation to date.
	 * 
	 * I also made the distortion tunable via config file, so the thing is sane
	 * and doesn't trash 8580 any more.
	 * <LI> 18.4.2007 Oops, a few months passed as I was busy at work.
	 * 
	 * I've got rid of the LEADDIST effect, simply accepted that things will not
	 * work with it. I hope to be able to replace it with another trick up my
	 * sleeve, but so far the new songs and patch probably constitute a step
	 * backwards. Maybe.
	 * <LI> 13.2.2007 Defeat with the masterful TBB songs such as 0_N_0 and
	 * Meanwhile The Planet. The 0_N_0 makes a lot of snapping because in a real
	 * chip the distortion terms are "filtered" by the other filter components,
	 * while mine are filtered by a static ad-hoc estimate. So, the snapping
	 * here comes from the LEADDIST term and it doesn't make the right kind of
	 * effect.
	 * 
	 * Meanwhile The Planet makes masterfully precise use of the filter, and
	 * there isn't enough distortion for ReSID rendition to be palatable. Thanks
	 * to Grue for digging these songs up. Also fixed invalid sidplay2.ini
	 * configuration file (was not using 6581R4 curve). This, too, was spotted
	 * by Grue.
	 * 
	 * I suspect Snake Disco might be fixed by accounting for the loss of
	 * intensity of sound with the Vbp op-amp distortion. I made some
	 * measurements last night and discovered that level of lp and hp outputs
	 * dropped dramatically when bp was driven to distortion. I observed loss of
	 * up to 20 dB. I probably have to make many more measurements to allow
	 * mapping an estimate for this behaviour.
	 * <LI> 12.2.2007 I added a minor tweak, adding an estimate of the resonance
	 * term in the op-amp loading measurement. I also made the bass distortion
	 * slightly more prominent, although it still isn't so harsh as the real
	 * chip produces.
	 * <LI> 11.2.2007 I decided the price for fixing RoboCop 3 on 10.2. changes
	 * was too high to pay. It put a few dB of distortion back and migrated the
	 * distortion back to later time. Other changes in frequency drift
	 * implementation however helped a lot even with RoboCop 3, although I do
	 * think it still distorts too much. I think overall I'm now better off.
	 * 
	 * I also moved the distortion term computation into the filter clocking
	 * function, so that if output is being requested at some different rate
	 * than the filter is clocked, the lowpass filtering would still be computed
	 * similarly. This bit me because I had this stupid OptimiseLevel
	 * PerformanceHacks accidentally enabled.
	 * <LI> 10.2.2007 I made the distortion term to start earlier but also more
	 * delicate. (Reduced distortion by 6 dB.) This fixed RoboCop_3, SYS4096 and
	 * improved locutox's cover of Purple Shades. (Not included on sample set.)
	 * I also set bass level to -4.4 dB, because it comes about 1.5 dB louder
	 * than treble or bandpass. This is in full contrast to comments in ReSID
	 * that claim no level difference exists. Strange, but I can see it plain as
	 * day on FFT.
	 * <LI> 3.2.2007 - 9.2.2007 Creation and tuning the model...
	 * </UL>
	 * @author Antti Lankila (alankila@bel.fi)
	 */
	public static boolean ANTTI_LANKILA_PATCH = true;

	/**
	 * Read/Write State.
	 * 
	 * @author Ken Händel
	 * 
	 */
	public class State {
		public char sid_register[] = new char[0x20];

		public int /* reg8 */bus_value;

		public int /* cycle_count */bus_value_ttl;

		public int /* reg24 */accumulator[] = new int[3];

		public int /* reg24 */shift_register[] = new int[3];

		public int /* reg16 */rate_counter[] = new int[3];

		public int /* reg16 */rate_counter_period[] = new int[3];

		public int /* reg16 */exponential_counter[] = new int[3];

		public int /* reg16 */exponential_counter_period[] = new int[3];

		int /* reg8 */envelope_counter[] = new int[3];

		public EnvelopeGenerator.State envelope_state[] = new EnvelopeGenerator.State[3];

		public boolean hold_zero[] = new boolean[3];

		/**
		 * Constructor.
		 */
		public State() {
			int i;

			for (i = 0; i < 0x20; i++) {
				sid_register[i] = 0;
			}

			bus_value = 0;
			bus_value_ttl = 0;

			for (i = 0; i < 3; i++) {
				accumulator[i] = 0;
				shift_register[i] = 0x7ffff8;
				rate_counter[i] = 0;
				rate_counter_period[i] = 9;
				exponential_counter[i] = 0;
				exponential_counter_period[i] = 1;
				envelope_counter[i] = 0;
				envelope_state[i] = EnvelopeGenerator.State.RELEASE;
				hold_zero[i] = true;
			}
		}

	}

	protected Voice voice[] = new Voice[] { new Voice(), new Voice(),
			new Voice() };

	public Filter filter = new Filter();

	protected ExternalFilter extfilt = new ExternalFilter();

	protected Potentiometer potx = new Potentiometer();

	protected Potentiometer poty = new Potentiometer();

	protected int /* reg8 */bus_value;

	protected int /* cycle_count */bus_value_ttl;

	protected double clock_frequency;

	/**
	 * External audio input.
	 */
	protected int ext_in;

	/**
	 * Resampling constants. The error in interpolated lookup is bounded by
	 * 1.234/L^2, while the error in non-interpolated lookup is bounded by
	 * 0.7854/L + 0.4113/L^2, see
	 * http://www-ccrma.stanford.edu/~jos/resample/Choice_Table_Size.html For a
	 * resolution of 16 bits this yields L >= 285 and L >= 51473, respectively.
	 */
	protected static final int FIR_N = 125;

	/**
	 * Resampling constants. The error in interpolated lookup is bounded by
	 * 1.234/L^2, while the error in non-interpolated lookup is bounded by
	 * 0.7854/L + 0.4113/L^2, see
	 * http://www-ccrma.stanford.edu/~jos/resample/Choice_Table_Size.html For a
	 * resolution of 16 bits this yields L >= 285 and L >= 51473, respectively.
	 */
	protected static final int FIR_RES_INTERPOLATE = 285;

	/**
	 * Resampling constants. The error in interpolated lookup is bounded by
	 * 1.234/L^2, while the error in non-interpolated lookup is bounded by
	 * 0.7854/L + 0.4113/L^2, see
	 * http://www-ccrma.stanford.edu/~jos/resample/Choice_Table_Size.html For a
	 * resolution of 16 bits this yields L >= 285 and L >= 51473, respectively.
	 */
	protected static final int FIR_RES_FAST = 51473;

	/**
	 * Resampling constants. The error in interpolated lookup is bounded by
	 * 1.234/L^2, while the error in non-interpolated lookup is bounded by
	 * 0.7854/L + 0.4113/L^2, see
	 * http://www-ccrma.stanford.edu/~jos/resample/Choice_Table_Size.html For a
	 * resolution of 16 bits this yields L >= 285 and L >= 51473, respectively.
	 */
	protected static final int FIR_SHIFT = 15;

	/**
	 * Resampling constants. The error in interpolated lookup is bounded by
	 * 1.234/L^2, while the error in non-interpolated lookup is bounded by
	 * 0.7854/L + 0.4113/L^2, see
	 * http://www-ccrma.stanford.edu/~jos/resample/Choice_Table_Size.html For a
	 * resolution of 16 bits this yields L >= 285 and L >= 51473, respectively.
	 */
	protected static final int RINGSIZE = 16384;

	/**
	 * Fixpoint constants (16.16 bits).
	 */
	protected static final int FIXP_SHIFT = 16;

	/**
	 * Fixpoint constants (16.16 bits).
	 */
	protected static final int FIXP_MASK = 0xffff;

	/**
	 * Sampling variables.
	 */
	protected ISIDDefs.sampling_method sampling;

	/**
	 * Sampling variables.
	 */
	protected int /* cycle_count */cycles_per_sample;

	/**
	 * Sampling variables.
	 */
	protected int /* cycle_count */sample_offset;

	/**
	 * Sampling variables.
	 */
	protected int sample_index;

	/**
	 * Sampling variables.
	 */
	protected short sample_prev;

	/**
	 * Sampling variables.
	 */
	protected int fir_N;

	/**
	 * Sampling variables.
	 */
	protected int fir_RES;

	/**
	 * Ring buffer with overflow for contiguous storage of RINGSIZE samples.
	 */
	protected short sample[];

	/**
	 * FIR_RES filter tables (FIR_N*FIR_RES).
	 */
	protected short fir[];

	/**
	 * Constructor.
	 */
	public SID() {
		// Initialize pointers.
		sample = null;
		fir = null;

		voice[0].set_sync_source(voice[2]);
		voice[1].set_sync_source(voice[0]);
		voice[2].set_sync_source(voice[1]);

		set_sampling_parameters(985248, ISIDDefs.sampling_method.SAMPLE_FAST, 44100, -1,
				0.97);

		bus_value = 0;
		bus_value_ttl = 0;

		ext_in = 0;
	}

	/**
	 * Set chip model.
	 * 
	 * @param model
	 */
	public void set_chip_model(ISIDDefs.chip_model model) {
		for (int i = 0; i < 3; i++) {
			voice[i].set_chip_model(model);
		}

		filter.set_chip_model(model);
		extfilt.set_chip_model(model);
	}

	public void set_distortion_properties(int Lt, int Ls, int Ll, int Lb,
			int Lh, int Ht, int Hs, int Hl, int Hb, int Hh) {
		filter
				.set_distortion_properties(Lt, Ls, Ll, Lb, Lh, Ht, Hs, Hl, Hb,
						Hh);
	}

	/**
	 * SID reset.
	 */
	public void reset() {
		for (int i = 0; i < 3; i++) {
			voice[i].reset();
		}
		filter.reset();
		extfilt.reset();

		bus_value = 0;
		bus_value_ttl = 0;
	}

	/**
	 * 16-bit input (EXT IN). Write 16-bit sample to audio input. NB! The caller
	 * is responsible for keeping the value within 16 bits. Note that to mix in
	 * an external audio signal, the signal should be resampled to 1MHz first to
	 * avoid sampling noise.
	 * 
	 * @param sample
	 */
	public void input(int sample) {
		// Voice outputs are 20 bits. Scale up to match three voices in order
		// to facilitate simulation of the MOS8580 "digi boost" hardware hack.
		ext_in = (sample << 4) * 3;
	}

	/**
	 * 16-bit output (AUDIO OUT). Read sample from audio output. Both 16-bit and
	 * n-bit output is provided.
	 * 
	 * @return
	 */
	public int output() {
		final int range = 1 << 16;
		final int half = range >> 1;
		int sample = extfilt.output()
				/ (((4095 * 255) >> 7) * 3 * 15 * 2 / range);
		if (sample >= half) {
			return half - 1;
		}
		if (sample < -half) {
			return -half;
		}
		return sample;
	}

	/**
	 * n-bit output.
	 * 
	 * @param bits
	 * @return
	 */
	public int output(int bits) {
		final int range = 1 << bits;
		final int half = range >> 1;
		int sample = extfilt.output()
				/ ((4095 * 255 >> 7) * 3 * 15 * 2 / range);
		if (sample >= half) {
			return half - 1;
		}
		if (sample < -half) {
			return -half;
		}
		return sample;
	}

	/**
	 * Read registers.
	 * <P>
	 * Reading a write only register returns the last byte written to any SID
	 * register. The individual bits in this value start to fade down towards
	 * zero after a few cycles. All bits reach zero within approximately $2000 -
	 * $4000 cycles. It has been claimed that this fading happens in an orderly
	 * fashion, however sampling of write only registers reveals that this is
	 * not the case. NB! This is not correctly modeled. The actual use of write
	 * only registers has largely been made in the belief that all SID registers
	 * are readable. To support this belief the read would have to be done
	 * immediately after a write to the same register (remember that an
	 * intermediate write to another register would yield that value instead).
	 * With this in mind we return the last value written to any SID register
	 * for $2000 cycles without modeling the bit fading.
	 * 
	 * @param offset
	 * @return
	 */
	public int /* reg8 */read(int /* reg8 */offset) {
		switch (offset) {
		case 0x19:
			return potx.readPOT();
		case 0x1a:
			return poty.readPOT();
		case 0x1b:
			return voice[2].wave.readOSC();
		case 0x1c:
			return voice[2].envelope.readENV();
		default:
			return bus_value;
		}
	}

	/**
	 * Write registers.
	 * 
	 * @param offset
	 * @param value
	 */
	public void write(int /* reg8 */offset, int /* reg8 */value) {
		bus_value = value;
		bus_value_ttl = 0x2000;

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
			voice[0].writeCONTROL_REG(value);
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
			voice[1].writeCONTROL_REG(value);
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
			voice[2].writeCONTROL_REG(value);
			break;
		case 0x13:
			voice[2].envelope.writeATTACK_DECAY(value);
			break;
		case 0x14:
			voice[2].envelope.writeSUSTAIN_RELEASE(value);
			break;
		case 0x15:
			filter.writeFC_LO(value);
			break;
		case 0x16:
			filter.writeFC_HI(value);
			break;
		case 0x17:
			filter.writeRES_FILT(value);
			break;
		case 0x18:
			filter.writeMODE_VOL(value);
			break;
		default:
			break;
		}
	}

	/**
	 * SID voice muting.
	 * 
	 * @param channel
	 * @param enable
	 */
	public void mute(int /* reg8 */channel, boolean enable) {
		// Only have 3 voices!
		if (channel >= 3)
			return;

		voice[channel].mute(enable);
	}

	/**
	 * Read state.
	 * 
	 * @return
	 */
	public State read_state() {
		State state = new State();
		int i, j;

		for (i = 0, j = 0; i < 3; i++, j += 7) {
			WaveformGenerator wave = voice[i].wave;
			EnvelopeGenerator envelope = voice[i].envelope;
			state.sid_register[j + 0] = (char) (wave.freq & 0xff);
			state.sid_register[j + 1] = (char) (wave.freq >> 8);
			state.sid_register[j + 2] = (char) (wave.pw & 0xff);
			state.sid_register[j + 3] = (char) (wave.pw >> 8);
			state.sid_register[j + 4] = (char) ((wave.waveform << 4)
					| ((wave.test != 0) ? 0x08 : 0)
					| ((wave.ring_mod != 0) ? 0x04 : 0)
					| ((wave.sync != 0) ? 0x02 : 0) | ((envelope.gate != 0) ? 0x01
					: 0));
			state.sid_register[j + 5] = (char) ((envelope.attack << 4) | envelope.decay);
			state.sid_register[j + 6] = (char) ((envelope.sustain << 4) | envelope.release);
		}

		state.sid_register[j++] = (char) (filter.fc & 0x007);
		state.sid_register[j++] = (char) (filter.fc >> 3);
		state.sid_register[j++] = (char) ((filter.res << 4) | filter.filt);
		state.sid_register[j++] = (char) (((filter.voice3off != 0) ? 0x80 : 0)
				| (filter.hp_bp_lp << 4) | filter.vol);

		// These registers are superfluous, but included for completeness.
		for (; j < 0x1d; j++) {
			state.sid_register[j] = (char) (read(j));
		}
		for (; j < 0x20; j++) {
			state.sid_register[j] = 0;
		}

		state.bus_value = bus_value;
		state.bus_value_ttl = bus_value_ttl;

		for (i = 0; i < 3; i++) {
			state.accumulator[i] = voice[i].wave.accumulator;
			state.shift_register[i] = voice[i].wave.shift_register;
			state.rate_counter[i] = voice[i].envelope.rate_counter;
			state.rate_counter_period[i] = voice[i].envelope.rate_period;
			state.exponential_counter[i] = voice[i].envelope.exponential_counter;
			state.exponential_counter_period[i] = voice[i].envelope.exponential_counter_period;
			state.envelope_counter[i] = voice[i].envelope.envelope_counter;
			state.envelope_state[i] = voice[i].envelope.state;
			state.hold_zero[i] = voice[i].envelope.hold_zero;
		}

		return state;
	}

	/**
	 * Write state.
	 * 
	 * @param state
	 */
	public void write_state(final State state) {
		int i;

		for (i = 0; i <= 0x18; i++) {
			write(i, state.sid_register[i]);
		}

		bus_value = state.bus_value;
		bus_value_ttl = state.bus_value_ttl;

		for (i = 0; i < 3; i++) {
			voice[i].wave.accumulator = state.accumulator[i];
			voice[i].wave.shift_register = state.shift_register[i];
			voice[i].envelope.rate_counter = state.rate_counter[i];
			voice[i].envelope.rate_period = state.rate_counter_period[i];
			voice[i].envelope.exponential_counter = state.exponential_counter[i];
			voice[i].envelope.exponential_counter_period = state.exponential_counter_period[i];
			voice[i].envelope.envelope_counter = state.envelope_counter[i];
			voice[i].envelope.state = state.envelope_state[i];
			voice[i].envelope.hold_zero = state.hold_zero[i];
		}
	}

	/**
	 * Enable filter.
	 * 
	 * @param enable
	 */
	public void enable_filter(boolean enable) {
		filter.enable_filter(enable);
	}

	/**
	 * Enable external filter.
	 * 
	 * @param enable
	 */
	public void enable_external_filter(boolean enable) {
		extfilt.enable_filter(enable);
	}

	/**
	 * I0() computes the 0th order modified Bessel function of the first kind.
	 * This function is originally from resample-1.5/filterkit.c by J. O. Smith.
	 * 
	 * @param x
	 * @return
	 */
	protected double I0(double x) {
		// Max error acceptable in I0.
		final double I0e = 1e-6;

		double sum, u, halfx, temp;
		int n;

		sum = u = n = 1;
		halfx = x / 2.0;

		do {
			temp = halfx / n++;
			u *= temp * temp;
			sum += u;
		} while (u >= I0e * sum);

		return sum;
	}

	/**
	 * Setting of SID sampling parameters.
	 * <P>
	 * Use a clock freqency of 985248Hz for PAL C64, 1022730Hz for NTSC C64. The
	 * default end of passband frequency is pass_freq = 0.9*sample_freq/2 for
	 * sample frequencies up to ~ 44.1kHz, and 20kHz for higher sample
	 * frequencies.
	 * <P>
	 * For resampling, the ratio between the clock frequency and the sample
	 * frequency is limited as follows: 125*clock_freq/sample_freq < 16384 E.g.
	 * provided a clock frequency of ~ 1MHz, the sample frequency can not be set
	 * lower than ~ 8kHz. A lower sample frequency would make the resampling
	 * code overfill its 16k sample ring buffer.
	 * <P>
	 * The end of passband frequency is also limited: pass_freq <=
	 * 0.9*sample_freq/2
	 * <P>
	 * E.g. for a 44.1kHz sampling rate the end of passband frequency is limited
	 * to slightly below 20kHz. This constraint ensures that the FIR table is
	 * not overfilled.
	 * 
	 * @param clock_freq
	 * @param method
	 * @param sample_freq
	 * @param pass_freq
	 * @param filter_scale
	 * @return
	 */
	public boolean set_sampling_parameters(double clock_freq,
			ISIDDefs.sampling_method method, double sample_freq, double pass_freq,
			double filter_scale) {
		// Check resampling constraints.
		if (method == ISIDDefs.sampling_method.SAMPLE_RESAMPLE_INTERPOLATE
				|| method == ISIDDefs.sampling_method.SAMPLE_RESAMPLE_FAST) {
			// Check whether the sample ring buffer would overfill.
			if (FIR_N * clock_freq / sample_freq >= RINGSIZE) {
				return false;
			}
		}
		// The default passband limit is 0.9*sample_freq/2 for sample
		// frequencies below ~ 44.1kHz, and 20kHz for higher sample
		// frequencies.
		if (pass_freq < 0) {
			pass_freq = 20000;
			if (2 * pass_freq / sample_freq >= 0.9) {
				pass_freq = 0.9 * sample_freq / 2;
			}
		}
		// Check whether the FIR table would overfill.
		else if (pass_freq > 0.9 * sample_freq / 2) {
			return false;
		}

		// The filter scaling is only included to avoid clipping, so keep
		// it sane.
		if (filter_scale < 0.9 || filter_scale > 1.0) {
			return false;
		}

		// Set the external filter to the pass freq
		extfilt.set_sampling_parameter(pass_freq);
		clock_frequency = clock_freq;
		sampling = method;

		cycles_per_sample = (int /* cycle_count */) (clock_freq / sample_freq
				* (1 << FIXP_SHIFT) + 0.5);

		sample_offset = 0;
		sample_prev = 0;

		// FIR initialization is only necessary for resampling.
		if (method != ISIDDefs.sampling_method.SAMPLE_RESAMPLE_INTERPOLATE
				&& method != ISIDDefs.sampling_method.SAMPLE_RESAMPLE_FAST) {
			sample = null;
			fir = null;
			return true;
		}

		final double pi = 3.1415926535897932385;

		// 16 bits -> -96dB stopband attenuation.
		final double A = -20 * Math.log10(1.0 / (1 << 16));
		// A fraction of the bandwidth is allocated to the transition band,
		double dw = (1 - 2 * pass_freq / sample_freq) * pi;
		// The cutoff frequency is midway through the transition band.
		double wc = (2 * pass_freq / sample_freq + 1) * pi / 2;

		// For calculation of beta and N see the reference for the kaiserord
		// function in the MATLAB Signal Processing Toolbox:
		// http://www.mathworks.com/access/helpdesk/help/toolbox/signal/kaiserord.html
		final double beta = 0.1102 * (A - 8.7);
		final double I0beta = I0(beta);

		// The filter order will maximally be 124 with the current constraints.
		// N >= (96.33 - 7.95)/(2.285*0.1*pi) -> N >= 123
		// The filter order is equal to the number of zero crossings, i.e.
		// it should be an even number (sinc is symmetric about x = 0).
		int N = (int) ((A - 7.95) / (2.285 * dw) + 0.5);
		N += N & 1;

		double f_samples_per_cycle = sample_freq / clock_freq;
		double f_cycles_per_sample = clock_freq / sample_freq;

		// The filter length is equal to the filter order + 1.
		// The filter length must be an odd number (sinc is symmetric about x =
		// 0).
		fir_N = (int) (N * f_cycles_per_sample) + 1;
		fir_N |= 1;

		// We clamp the filter table resolution to 2^n, making the fixpoint
		// sample_offset a whole multiple of the filter table resolution.
		int res = method == ISIDDefs.sampling_method.SAMPLE_RESAMPLE_INTERPOLATE ? FIR_RES_INTERPOLATE
				: FIR_RES_FAST;
		int n = (int) Math.ceil(Math.log(res / f_cycles_per_sample)
				/ Math.log((double) 2));
		fir_RES = 1 << n;

		// Allocate memory for FIR tables.
		fir = null;
		fir = new short[fir_N * fir_RES];

		// Calculate fir_RES FIR tables for linear interpolation.
		for (int i = 0; i < fir_RES; i++) {
			int fir_offset = i * fir_N + fir_N / 2;
			double j_offset = (double) (i) / fir_RES;
			// Calculate FIR table. This is the sinc function, weighted by the
			// Kaiser window.
			for (int j = -fir_N / 2; j <= fir_N / 2; j++) {
				double jx = j - j_offset;
				double wt = wc * jx / f_cycles_per_sample;
				double temp = jx / (fir_N / 2);
				double Kaiser = Math.abs(temp) <= 1 ? I0(beta
						* Math.sqrt(1 - temp * temp))
						/ I0beta : 0;
				double sincwt = Math.abs(wt) >= 1e-6 ? Math.sin(wt) / wt : 1;
				double val = (1 << FIR_SHIFT) * filter_scale
						* f_samples_per_cycle * wc / pi * sincwt * Kaiser;
				fir[fir_offset + j] = (short) (val + 0.5);
			}
		}

		// Allocate sample buffer.
		if ((sample == null)) {
			sample = new short[RINGSIZE * 2];
		}
		// Clear sample buffer.
		for (int j = 0; j < RINGSIZE * 2; j++) {
			sample[j] = 0;
		}
		sample_index = 0;

		return true;
	}

	/**
	 * Adjustment of SID sampling frequency.
	 * <P>
	 * In some applications, e.g. a C64 emulator, it can be desirable to
	 * synchronize sound with a timer source. This is supported by adjustment of
	 * the SID sampling frequency.
	 * <P>
	 * NB! Adjustment of the sampling frequency may lead to noticeable shifts in
	 * frequency, and should only be used for interactive applications. Note
	 * also that any adjustment of the sampling frequency will change the
	 * characteristics of the resampling filter, since the filter is not
	 * rebuilt.
	 * 
	 * @param sample_freq
	 */
	public void adjust_sampling_frequency(double sample_freq) {
		cycles_per_sample = (int /* cycle_count */) (clock_frequency
				/ sample_freq * (1 << FIXP_SHIFT) + 0.5);
	}

	// ----------------------------------------------------------------------------
	// Return array of default spline interpolation points to map FC to
	// filter cutoff frequency.
	// ----------------------------------------------------------------------------
	public class FCPoints {
		public int[] /* fc_point */[] points;

		public int count;

		public FCPoints() {
		}
	}

	public void fc_default(FCPoints fcp) {
		filter.fc_default(fcp);
	}

	/**
	 * Return FC spline plotter object.
	 * 
	 * @return
	 */
	public PointPlotter fc_plotter() {
		return filter.fc_plotter();
	}

	/**
	 * SID clocking - 1 cycle.
	 */
	public void clock() {
		int i;

		// Age bus value.
		if (--bus_value_ttl <= 0) {
			bus_value = 0;
			bus_value_ttl = 0;
		}

		// Clock amplitude modulators.
		for (i = 0; i < 3; i++) {
			voice[i].envelope.clock();
		}

		// Clock oscillators.
		for (i = 0; i < 3; i++) {
			voice[i].wave.clock();
		}

		// Synchronize oscillators.
		for (i = 0; i < 3; i++) {
			voice[i].wave.synchronize();
		}

		// Clock filter.
		filter.clock(voice[0].output(), voice[1].output(), voice[2].output(),
				ext_in);

		// Clock external filter.
		extfilt.clock(filter.output());
	}

	/**
	 * SID clocking - delta_t cycles.
	 * 
	 * @param delta_t
	 */
	public void clock(int /* cycle_count */delta_t) {
		int i;

		if (delta_t <= 0) {
			return;
		}

		// Age bus value.
		bus_value_ttl -= delta_t;
		if (bus_value_ttl <= 0) {
			bus_value = 0;
			bus_value_ttl = 0;
		}

		// Clock amplitude modulators.
		for (i = 0; i < 3; i++) {
			voice[i].envelope.clock(delta_t);
		}

		// Clock and synchronize oscillators.
		// Loop until we reach the current cycle.
		int /* cycle_count */delta_t_osc = delta_t;
		while (delta_t_osc != 0) {
			int /* cycle_count */delta_t_min = delta_t_osc;

			// Find minimum number of cycles to an oscillator accumulator MSB
			// toggle.
			// We have to clock on each MSB on / MSB off for hard sync to
			// operate
			// correctly.
			for (i = 0; i < 3; i++) {
				WaveformGenerator wave = voice[i].wave;

				// It is only necessary to clock on the MSB of an oscillator
				// that is
				// a sync source and has freq != 0.
				if (!((wave.sync_dest.sync != 0) && (wave.freq != 0))) {
					continue;
				}

				int /* reg16 */freq = wave.freq;
				int /* reg24 */accumulator = wave.accumulator;

				// Clock on MSB off if MSB is on, clock on MSB on if MSB is off.
				int /* reg24 */delta_accumulator = ((accumulator & 0x800000) != 0 ? 0x1000000
						: 0x800000)
						- accumulator;

				int /* cycle_count */delta_t_next = (delta_accumulator / freq);
				if ((delta_accumulator % freq) != 0) {
					++delta_t_next;
				}

				if (delta_t_next < delta_t_min) {
					delta_t_min = delta_t_next;
				}
			}

			// Clock oscillators.
			for (i = 0; i < 3; i++) {
				voice[i].wave.clock(delta_t_min);
			}

			// Synchronize oscillators.
			for (i = 0; i < 3; i++) {
				voice[i].wave.synchronize();
			}

			delta_t_osc -= delta_t_min;
		}

		// Clock filter.
		filter.clock(delta_t, voice[0].output(), voice[1].output(), voice[2]
				.output(), ext_in);

		// Clock external filter.
		extfilt.clock(delta_t, filter.output());
	}

	public class CycleCount {
		public CycleCount(int delta_t2) {
			delta_t = delta_t2;
		}

		public int /* cycle_count */delta_t;
	}

	/**
	 * SID clocking with audio sampling. Fixpoint arithmetics is used.
	 * <P>
	 * The example below shows how to clock the SID a specified amount of cycles
	 * while producing audio output:
	 * 
	 * <pre>
	 * while (delta_t) {
	 * 	bufindex += sid.clock(delta_t, buf + bufindex, buflength - bufindex);
	 * 	write(dsp, buf, bufindex * 2);
	 * 	bufindex = 0;
	 * }
	 * </pre>
	 * 
	 * @return
	 */
	public int clock(CycleCount delta_t, short buf[], int n, int interleave) {
		switch (sampling) {
		default:
		case SAMPLE_FAST:
			return clock_fast(delta_t, buf, n, interleave);
		case SAMPLE_INTERPOLATE:
			return clock_interpolate(delta_t, buf, n, interleave);
		case SAMPLE_RESAMPLE_INTERPOLATE:
			return clock_resample_interpolate(delta_t, buf, n, interleave);
		case SAMPLE_RESAMPLE_FAST:
			return clock_resample_fast(delta_t, buf, n, interleave);
		}
	}

	/**
	 * SID clocking with audio sampling - delta clocking picking nearest sample.
	 * 
	 * @return
	 */
	protected int clock_fast(CycleCount delta_t, short buf[], int n,
			int interleave) {
		int s = 0;

		for (;;) {
			int /* cycle_count */next_sample_offset = sample_offset
					+ cycles_per_sample + (1 << (FIXP_SHIFT - 1));
			int /* cycle_count */delta_t_sample = next_sample_offset >> FIXP_SHIFT;
			if (delta_t_sample > delta_t.delta_t) {
				break;
			}
			if (s >= n) {
				return s;
			}
			clock(delta_t_sample);
			delta_t.delta_t -= delta_t_sample;
			sample_offset = (next_sample_offset & FIXP_MASK)
					- (1 << (FIXP_SHIFT - 1));
			buf[s++ * interleave] = (short) output();
		}

		clock(delta_t.delta_t);
		sample_offset -= delta_t.delta_t << FIXP_SHIFT;
		delta_t.delta_t = 0;
		return s;
	}

	/**
	 * SID clocking with audio sampling - cycle based with linear sample
	 * interpolation.
	 * <P>
	 * Here the chip is clocked every cycle. This yields higher quality sound
	 * since the samples are linearly interpolated, and since the external
	 * filter attenuates frequencies above 16kHz, thus reducing sampling noise.
	 * 
	 * @return
	 */
	protected int clock_interpolate(CycleCount delta_t, short buf[], int n,
			int interleave) {
		int s = 0;
		int i;

		for (;;) {
			int /* cycle_count */next_sample_offset = sample_offset
					+ cycles_per_sample;
			int /* cycle_count */delta_t_sample = next_sample_offset >> FIXP_SHIFT;
			if (delta_t_sample > delta_t.delta_t) {
				break;
			}
			if (s >= n) {
				return s;
			}
			for (i = 0; i < delta_t_sample - 1; i++) {
				clock();
			}
			if (i < delta_t_sample) {
				sample_prev = (short) output();
				clock();
			}

			delta_t.delta_t -= delta_t_sample;
			sample_offset = next_sample_offset & FIXP_MASK;

			short sample_now = (short) output();
			buf[s++ * interleave] = (short) (sample_prev + (sample_offset
					* (sample_now - sample_prev) >> FIXP_SHIFT));
			sample_prev = sample_now;
		}

		for (i = 0; i < delta_t.delta_t - 1; i++) {
			clock();
		}
		if (i < delta_t.delta_t) {
			sample_prev = (short) output();
			clock();
		}
		sample_offset -= delta_t.delta_t << FIXP_SHIFT;
		delta_t.delta_t = 0;
		return s;
	}

	/**
	 * SID clocking with audio sampling - cycle based with audio resampling.
	 * <P>
	 * This is the theoretically correct (and computationally intensive) audio
	 * sample generation. The samples are generated by resampling to the
	 * specified sampling frequency. The work rate is inversely proportional to
	 * the percentage of the bandwidth allocated to the filter transition band.
	 * <P>
	 * This implementation is based on the paper "A Flexible Sampling-Rate
	 * Conversion Method", by J. O. Smith and P. Gosset, or rather on the
	 * expanded tutorial on the "Digital Audio Resampling Home Page":
	 * http://www-ccrma.stanford.edu/~jos/resample/
	 * <P>
	 * By building shifted FIR tables with samples according to the sampling
	 * frequency, this implementation dramatically reduces the computational
	 * effort in the filter convolutions, without any loss of accuracy. The
	 * filter convolutions are also vectorizable on current hardware.
	 * <P>
	 * Further possible optimizations are: * An equiripple filter design could
	 * yield a lower filter order, see
	 * http://www.mwrf.com/Articles/ArticleID/7229/7229.html * The Convolution
	 * Theorem could be used to bring the complexity of convolution down from
	 * O(n*n) to O(n*log(n)) using the Fast Fourier Transform, see
	 * http://en.wikipedia.org/wiki/Convolution_theorem * Simply resampling in
	 * two steps can also yield computational savings, since the transition band
	 * will be wider in the first step and the required filter order is thus
	 * lower in this step. Laurent Ganier has found the optimal intermediate
	 * sampling frequency to be (via derivation of sum of two steps): 2 *
	 * pass_freq + sqrt [ 2 * pass_freq * orig_sample_freq * (dest_sample_freq -
	 * 2 * pass_freq) / dest_sample_freq ]
	 * <P>
	 * 
	 * @return
	 */
	protected int clock_resample_interpolate(CycleCount delta_t, short buf[],
			int n, int interleave) {
		int s = 0;

		for (;;) {
			int /* cycle_count */next_sample_offset = sample_offset
					+ cycles_per_sample;
			int /* cycle_count */delta_t_sample = next_sample_offset >> FIXP_SHIFT;
			if (delta_t_sample > delta_t.delta_t) {
				break;
			}
			if (s >= n) {
				return s;
			}
			for (int i = 0; i < delta_t_sample; i++) {
				clock();
				sample[sample_index] = sample[sample_index + RINGSIZE] = (short) output();
				++sample_index;
				sample_index &= 0x3fff;
			}
			delta_t.delta_t -= delta_t_sample;
			sample_offset = next_sample_offset & FIXP_MASK;

			int fir_offset = sample_offset * fir_RES >> FIXP_SHIFT;
			int fir_offset_rmd = sample_offset * fir_RES & FIXP_MASK;
			int fir_start = (fir_offset * fir_N);
			int sample_start = (sample_index - fir_N + RINGSIZE);

			// Convolution with filter impulse response.
			int v1 = 0;
			for (int j = 0; j < fir_N; j++) {
				v1 += sample[sample_start + j] * fir[fir_start + j];
			}

			// Use next FIR table, wrap around to first FIR table using
			// previous sample.
			if (++fir_offset == fir_RES) {
				fir_offset = 0;
				--sample_start;
			}
			fir_start = (fir_offset * fir_N);

			// Convolution with filter impulse response.
			int v2 = 0;
			for (int j = 0; j < fir_N; j++) {
				v2 += sample[sample_start + j] * fir[fir_start + j];
			}

			// Linear interpolation.
			// fir_offset_rmd is equal for all samples, it can thus be
			// factorized out:
			// sum(v1 + rmd*(v2 - v1)) = sum(v1) + rmd*(sum(v2) - sum(v1))
			int v = v1 + (fir_offset_rmd * (v2 - v1) >> FIXP_SHIFT);

			v >>= FIR_SHIFT;

			// Saturated arithmetics to guard against 16 bit sample overflow.
			final int half = 1 << 15;
			if (v >= half) {
				v = half - 1;
			} else if (v < -half) {
				v = -half;
			}

			buf[s++ * interleave] = (short) v;
		}

		for (int i = 0; i < delta_t.delta_t; i++) {
			clock();
			sample[sample_index] = sample[sample_index + RINGSIZE] = (short) output();
			++sample_index;
			sample_index &= 0x3fff;
		}
		sample_offset -= delta_t.delta_t << FIXP_SHIFT;
		delta_t.delta_t = 0;
		return s;
	}

	/**
	 * SID clocking with audio sampling - cycle based with audio resampling.
	 * 
	 * @return
	 */
	protected int clock_resample_fast(CycleCount delta_t, short buf[], int n,
			int interleave) {
		int s = 0;

		for (;;) {
			int /* cycle_count */next_sample_offset = sample_offset
					+ cycles_per_sample;
			int /* cycle_count */delta_t_sample = next_sample_offset >> FIXP_SHIFT;
			if (delta_t_sample > delta_t.delta_t) {
				break;
			}
			if (s >= n) {
				return s;
			}
			for (int i = 0; i < delta_t_sample; i++) {
				clock();
				sample[sample_index] = sample[sample_index + RINGSIZE] = (short) output();
				++sample_index;
				sample_index &= 0x3fff;
			}
			delta_t.delta_t -= delta_t_sample;
			sample_offset = next_sample_offset & FIXP_MASK;

			int fir_offset = sample_offset * fir_RES >> FIXP_SHIFT;
			int fir_start = (fir_offset * fir_N);
			int sample_start = (sample_index - fir_N + RINGSIZE);

			// Convolution with filter impulse response.
			int v = 0;
			for (int j = 0; j < fir_N; j++) {
				v += sample[sample_start + j] * fir[fir_start + j];
			}

			v >>= FIR_SHIFT;

			// Saturated arithmetics to guard against 16 bit sample overflow.
			final int half = 1 << 15;
			if (v >= half) {
				v = half - 1;
			} else if (v < -half) {
				v = -half;
			}

			buf[s++ * interleave] = (short) v;
		}

		for (int i = 0; i < delta_t.delta_t; i++) {
			clock();
			sample[sample_index] = sample[sample_index + RINGSIZE] = (short) output();
			++sample_index;
			sample_index &= 0x3fff;
		}
		sample_offset -= delta_t.delta_t << FIXP_SHIFT;
		delta_t.delta_t = 0;
		return s;
	}
}
