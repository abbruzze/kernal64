package ucesoft.c64.peripheral.sid.resid2;

/**
 * This class solves the opamp equation when loaded by different sets of resistors.
 * Equations and first implementation were written by Dag Lem.
 * This class is a rewrite without use of fixed point integer mathematics, and
 * uses the actual voltages instead of the normalized values.
 * 
 * @author alankila
 */
final class OpAmp {
	private final double EPSILON = 1e-8;
	
	/** Current root position (cached as guess to speed up next iteration) */
	private double x;

	private final double Vddt, vmin, vmax;
	
	protected final Spline opamp;
	
	final double[] out = new double[2];

	/**
	 * Opamp input -> output voltage conversion
	 * 
	 * @param opamp opamp mapping table as pairs of points (in -> out)
	 * @param Vddt transistor dt parameter (in volts)
	 */
	protected OpAmp(double[][] opamp, double Vddt) {
		this.Vddt = Vddt;
		this.vmin = opamp[0][0];
		this.vmax = opamp[opamp.length - 1][0];
		this.opamp = new Spline(opamp);
	}

	protected void reset() {
		x = vmin;
	}
	
	/**
	 * Solve the opamp equation for input vi in loading context n
	 * 
	 * @param n the ratio of input/output loading
	 * @param vi input
	 * @return vo
	 */
	protected double solve(double n, double vi) {
		// Start off with an estimate of x and a root bracket [ak, bk].
		// f is decreasing, so that f(ak) > 0 and f(bk) < 0.
		double ak = vmin;
		double bk = vmax;

		double a = n + 1;
		double b = Vddt;
		double b_vi = (b - vi);
		double c = n * (b_vi * b_vi);
		
		while (true) {
			double xk = x;
			
			// Calculate f and df.
			opamp.evaluate(x, out);
			double vo = out[0];
			double dvo = out[1];
			
			double b_vx = b - x;
			double b_vo = b - vo;

			double f = a * (b_vx * b_vx) - c - (b_vo * b_vo);
			double df = 2. * (b_vo * dvo - a * b_vx);

			x -= f / df;
			if (Math.abs(x - xk) < EPSILON) {
				opamp.evaluate(x, out);
				return out[0];
			}

		    // Narrow down root bracket.
		    if (f < 0) {
		      // f(xk) < 0
		      bk = xk;
		    }
		    else {
		      // f(xk) > 0
		      ak = xk;
		    }

		    if (x <= ak || x >= bk) {
		      // Bisection step (ala Dekker's method).
		      x = (ak + bk) * 0.5;
		    }
		}
	}
}