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
 * Potentiometer representation.
 * 
 * This class will probably never be implemented in any real way.
 * 
 * @author Ken Handel
 * @author Dag Lem
 */
final class Potentiometer {
	/**
	 * Read paddle value. Not modeled.
	 *
	 * @return paddle value (always 0xff)
	 */
	protected byte readPOT() {
		return (byte) 0xff;
	}
}
