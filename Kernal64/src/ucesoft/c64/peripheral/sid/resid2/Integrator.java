package ucesoft.c64.peripheral.sid.resid2;


/**
 * Java port of the reSID 1.0 filter VCR+opamp+capacitor element (integrator) by Dag Lem.
 * 
 * Java port and subthreshold current simulation added by Antti S. Lankila.
 * 
 * @author Antti S. Lankila
 * @author Dag Lem
 */
final class Integrator {
	private int Vddt_Vw_2, Vddt, n_snake, x, vc;
	private final char[] vcr_Vg, vcr_n_Ids_term, opamp_rev;

	protected Integrator(char[] vcr_Vg, char[] vcr_n_Ids_term, char[] opamp_rev, int Vddt, int n_snake) {
		this.vcr_Vg = vcr_Vg;
		this.vcr_n_Ids_term = vcr_n_Ids_term;
		this.opamp_rev = opamp_rev;
		
		this.Vddt = Vddt;
		this.n_snake = n_snake;
	}
	
	protected void setVw(int Vw) {
		Vddt_Vw_2 = (Vddt - Vw) * (Vddt - Vw) >>> 1;
	}
	
	protected int solve(final int vi) {
		// "Snake" voltages for triode mode calculation.
		int Vgst = Vddt - x;
		int Vgdt = Vddt - vi;
		int Vgdt_2 = Vgdt*Vgdt;

		// "Snake" current, scaled by (1/m)*2^13*m*2^16*m*2^16*2^-15 = m*2^30
		int n_I_snake = n_snake*((Vgst*Vgst >>> 15) - (Vgdt_2 >>> 15));

		// VCR gate voltage.       // Scaled by m*2^16
		// Vg = Vddt - sqrt(((Vddt - Vw)^2 + Vgdt^2)/2)
		int Vg = vcr_Vg[(Vddt_Vw_2 >>> 16) + (Vgdt_2 >>> 17)];

		// VCR voltages for EKV model table lookup.
		int Vgs = Vg - x;
		if (Vgs < 0) {
			Vgs = 0;
		}
		int Vgd = Vg - vi;
		if (Vgd < 0) {
			Vgd = 0;
		}

		// VCR current, scaled by m*2^15*2^15 = m*2^30
		int n_I_vcr = (vcr_n_Ids_term[Vgs & 0xffff] - vcr_n_Ids_term[Vgd & 0xffff]) << 15;

		// Change in capacitor charge.
		vc += n_I_snake + n_I_vcr;

		// vx = g(vc)
		x = opamp_rev[(vc >> 15) + (1 << 15) & 0xffff];

		// Return vo.
		return x - (vc >> 14);
	}
}