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
package ucesoft.c64.peripheral.sid.resid2;

import java.util.HashMap;
import java.util.Map;


/**
 * Combined waveform calculator for WaveformGenerator.
 * 
 * @author Antti Lankila
 */
public final class WaveformCalculator {
	private static Map<CombinedWaveformConfig[], short[][]> CACHE = new HashMap<CombinedWaveformConfig[], short[][]>();

	protected static class CombinedWaveformConfig {
		protected CombinedWaveformConfig(final float f, final float g, final float h, final float i, final float j) {
			bias = f;
			pulsestrength = g;
			topbit = h;
			distance = i;
			stmix = j;
		}

		protected float bias;
		protected float pulsestrength;
		protected float topbit;
		protected float distance;
		protected float stmix;
	}

	/*
	 * the "bits wrong" figures below are not directly comparable. 0 bits are very easy to predict, and waveforms that are mostly zero have low scores. More comparable scores would be found by
	 * dividing with the count of 1-bits, or something.
	 */
	private static final CombinedWaveformConfig config[][] = new CombinedWaveformConfig[][] {
		{ /* kevtris chip G (6581r2/r3) */
			new CombinedWaveformConfig(0.880815f, 0f, 0f, 0.3279614f, 0.5999545f), // error 1795
			new CombinedWaveformConfig(0.8924618f, 2.014781f, 1.003332f, 0.02992322f, 0.0f), // error 11610
			new CombinedWaveformConfig(0.8646501f, 1.712586f, 1.137704f, 0.02845423f, 0f), // error 21307
			new CombinedWaveformConfig(0.9527834f, 1.794777f, 0f, 0.09806272f, 0.7752482f), // error 196
		}, { /* kevtris chip V (8580) */
			new CombinedWaveformConfig(0.9781665f, 0f, 0.9899469f, 8.087667f, 0.8226412f), // error 5546
			new CombinedWaveformConfig(0.9097769f, 2.039997f, 0.9584096f, 0.1765447f, 0f), // error 18763
			new CombinedWaveformConfig(0.9231212f, 2.084788f, 0.9493895f, 0.1712518f, 0f), // error 17103
			new CombinedWaveformConfig(0.9845552f, 1.415612f, 0.9703883f, 3.68829f, 0.8265008f), // error 3319
		},
	};

	/**
	 * Build waveform tables for use by WaveformGenerator. The method returns 3
	 * tables in an Object[] wrapper:
	 * 
	 * 1. float[11][4096] wftable: the analog values in the waveform table
	 * 2. float[12] dac table for values of the nonlinear bits used in waveforms.
	 * 3. byte[11][4096] wfdigital: the digital values in the waveform table.
	 * 
	 * The wf* tables are structured as follows: indices 0 .. 6 correspond
	 * to SID waveforms of 1 to 7 with pulse width value set to 0x1000 (never
	 * triggered). Indices 7 .. 10 correspond to the pulse waveforms with
	 * width set to 0x000 (always triggered).
	 * 
	 * @param model Chip model to use
	 * @return Table suite
	 */
	protected static short[][] buildTable(ChipModel model) {
		CombinedWaveformConfig[] cfgArray = config[model == ChipModel.MOS6581 ? 0 : 1];
		
		if (! CACHE.containsKey(cfgArray)) {
			short[][] wftable = new short[8][4096];
			
			for (int accumulator = 0; accumulator < 1 << 24; accumulator += 1 << 12) {
				int idx = (accumulator >> 12);
				wftable[0][idx] = 0xfff;
				wftable[1][idx] = (short) ((accumulator & 0x800000) == 0 ? idx << 1 : (idx ^ 0xfff) << 1);
				wftable[2][idx] = (short) idx;
				wftable[3][idx] = calculateCombinedWaveform(cfgArray[0], 3, accumulator);
				wftable[4][idx] = 0xfff;
				wftable[5][idx] = calculateCombinedWaveform(cfgArray[1], 5, accumulator);
				wftable[6][idx] = calculateCombinedWaveform(cfgArray[2], 6, accumulator);
				wftable[7][idx] = calculateCombinedWaveform(cfgArray[3], 7, accumulator);
			}

			CACHE.put(cfgArray, wftable);
		}

		return CACHE.get(cfgArray);
	}

	/**
	 * Generate bitstate based on emulation of combined waves.
	 * 
	 * @param config the {@link CombinedWaveformConfig} to use.
	 * @param waveform the waveform to emulate, 1 .. 7
	 * @param accumulator the accumulator value.
	 */
	private static short calculateCombinedWaveform(CombinedWaveformConfig config, int waveform, int accumulator) {
		final float[] o = new float[12];
		
		/* S with strong top bit for 6581 */
		for (int i = 0; i < 12; i++) {
			o[i] = (accumulator >> 12 & (1 << i)) != 0 ? 1f : 0f;
		}

		/* convert to T */
		if ((waveform & 3) == 1) {
			final boolean top = (accumulator & 0x800000) != 0;
			for (int i = 11; i > 0; i--) {
				if (top) {
					o[i] = 1.0f - o[i - 1];
				} else {
					o[i] = o[i - 1];
				}
			}
			o[0] = 0;
		}

		/* convert to ST */
		if ((waveform & 3) == 3) {
			/* bottom bit is grounded via T waveform selector */
			o[0] *= config.stmix;
			for (int i = 1; i < 12; i++) {
				o[i] = o[i - 1] * (1.f - config.stmix) + o[i] * config.stmix;
			}
		}

		o[11] *= config.topbit;

		/* ST, P* waveform? */
		if (waveform == 3 || waveform > 4) {
			final float[] distancetable = new float[12 * 2 + 1];
			for (int i = 0; i <= 12; i++) {
				distancetable[12 + i] = distancetable[12 - i] = 1.f / (1.f + i * i * config.distance);
			}

			final float[] tmp = new float[12];
			for (int i = 0; i < 12; i++) {
				float avg = 0;
				float n = 0;
				for (int j = 0; j < 12; j++) {
					final float weight = distancetable[i - j + 12];
					avg += o[j] * weight;
					n += weight;
				}
				/* pulse control bit */
				if (waveform > 4) {
					final float weight = distancetable[i - 12 + 12];
					avg += config.pulsestrength * weight;
					n += weight;
				}

				tmp[i] = (o[i] + avg / n) * 0.5f;
			}

			for (int i = 0; i < 12; i++) {
				o[i] = tmp[i];
			}
		}

		short value = 0;
		for (int i = 0; i < 12; i++) {
			if (o[i] - config.bias > 0) {
				value |= 1 << i;
			}
		}
		
		return value;
	}
}