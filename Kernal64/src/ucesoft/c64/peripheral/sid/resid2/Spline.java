package ucesoft.c64.peripheral.sid.resid2;

final class Spline {
	private final double[][] params;
	private double[] c;
	
	protected Spline(double[][] input) {
		params = new double[input.length - 1][];

		for (int i = 0; i < input.length - 1; i ++) {
			double[] p0 = i != 0 ? input[i-1] : null;
			double[] p1 = input[i];
			double[] p2 = input[i+1];
			double[] p3 = i != input.length - 2 ? input[i+2] : null;

			final double k1, k2;
			if (p0 == null) {
				k2 = (p3[1] - p1[1])/(p3[0] - p1[0]);
				k1 = (3*(p2[1] - p1[1])/(p2[0] - p1[0]) - k2)/2;
			} else if (p3 == null) {
				k1 = (p2[1] - p0[1])/(p2[0] - p0[0]);
				k2 = (3*(p2[1] - p1[1])/(p2[0] - p1[0]) - k1)/2;
			} else {
				k1 = (p2[1] - p0[1])/(p2[0] - p0[0]);
				k2 = (p3[1] - p1[1])/(p3[0] - p1[0]);
			}

			double x1 = p1[0];
			double y1 = p1[1];
			double x2 = p2[0];
			double y2 = p2[1];
			
			double dx = x2 - x1;
			double dy = y2 - y1;

			double a = ((k1 + k2) - 2*dy/dx)/(dx*dx);
			double b = ((k2 - k1)/dx - 3*(x1 + x2)*a)/2;
			double c = k1 - (3*x1*a + 2*b)*x1;
			double d = y1 - ((x1*a + b)*x1 + c)*x1;
			
			params[i] = new double[] {
				x1, x2, a, b, c, d
			};
		}
		
		/* Fix the value ranges, because we interpolate outside original bounds if necessary. */
		params[0][0] = Double.MIN_VALUE;
		params[params.length-1][1] = Double.MAX_VALUE;

		c = params[0];
	}
	
	protected void evaluate(double x, double[] out) {
		if (x < c[0] || x > c[1]) {
			for (int i = 0; i < params.length; i ++) {
				if (params[i][1] < x) {
					continue;
				}
				c = params[i];
				break;
			}
		}
		
		double y = ((c[2]*x + c[3])*x + c[4])*x + c[5];
		double yd = (3*c[2]*x + 2*c[3])*x + c[4];
		out[0] = y;
		out[1] = yd;
	}
}