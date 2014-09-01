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

/**
 * Class for plotting integers into an array.
 * 
 * @author Ken Händel
 * 
 */
public class PointPlotter {

	protected int /* sound_sample */f[];

	public PointPlotter(int /* sound_sample */arr[]) {
		this.f = arr;
	}

	void plot(double x, double y) {
		// Clamp negative values to zero.
		if (y < 0) {
			y = 0;
		}

		f[(int) x] = (int /* sound_sample */) y;
	}
}
