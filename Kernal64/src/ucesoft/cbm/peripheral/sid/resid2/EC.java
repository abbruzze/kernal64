package ucesoft.cbm.peripheral.sid.resid2;

class EC {
    static int[] build_dac_table(final int bits, final double _2R_div_R, final boolean term) {
        final double[] vbit = new double[bits];
        final int[] dac = new int[1 << bits];
        for (int set_bit = 0; set_bit < bits; ++set_bit) {
            double Vn = 1.0;
            final double R = 1.0;
            final double _2R = _2R_div_R * R;
            double Rn = term ? _2R : Double.POSITIVE_INFINITY;
            int bit;
            for (bit = 0; bit < set_bit; ++bit) {
                if (Rn == Double.POSITIVE_INFINITY) {
                    Rn = R + _2R;
                } else {
                    Rn = R + _2R * Rn / (_2R + Rn);
                }
            }
            if (Rn == Double.POSITIVE_INFINITY) {
                Rn = _2R;
            } else {
                Rn = _2R * Rn / (_2R + Rn);
                Vn = Vn * Rn / _2R;
            }
            ++bit;
            while (bit < bits) {
                Rn += R;
                final double I = Vn / Rn;
                Rn = _2R * Rn / (_2R + Rn);
                Vn = Rn * I;
                ++bit;
            }
            vbit[set_bit] = Vn;
        }
        for (int i = 0; i < 1 << bits; ++i) {
            int x = i;
            double Vo = 0.0;
            for (int j = 0; j < bits; ++j) {
                Vo += (x & 0x1) * vbit[j];
                x >>= 1;
            }
            dac[i] = ((int) (((1 << bits) - 1) * Vo + 0.5) & 0xFFFF);
        }
        return dac;
    }

    private static void cubic_coefficients(final double x1, final double y1, final double x2, final double y2, final double k1, final double k2, final Coefficients coeff) {
        final double dx = x2 - x1;
        final double dy = y2 - y1;
        coeff.a = (k1 + k2 - 2.0 * dy / dx) / (dx * dx);
        coeff.b = ((k2 - k1) / dx - 3.0 * (x1 + x2) * coeff.a) / 2.0;
        coeff.c = k1 - (3.0 * x1 * coeff.a + 2.0 * coeff.b) * x1;
        coeff.d = y1 - ((x1 * coeff.a + coeff.b) * x1 + coeff.c) * x1;
    }

    private static void interpolate_forward_difference(final double x1, final double y1, final double x2, final double y2, final double k1, final double k2, final PointPlotter plotter, final double res) {
        final Coefficients coeff = new Coefficients();
        cubic_coefficients(x1, y1, x2, y2, k1, k2, coeff);
        double y3 = ((coeff.a * x1 + coeff.b) * x1 + coeff.c) * x1 + coeff.d;
        double dy = (3.0 * coeff.a * (x1 + res) + 2.0 * coeff.b) * x1 * res + ((coeff.a * res + coeff.b) * res + coeff.c) * res;
        double d2y = (6.0 * coeff.a * (x1 + res) + 2.0 * coeff.b) * res * res;
        final double d3y = 6.0 * coeff.a * res * res * res;
        for (double x3 = x1; x3 <= x2; x3 += res) {
            plotter.plot(x3, y3);
            y3 += dy;
            dy += d2y;
            d2y += d3y;
        }
    }

    private static double x(final int[][] f0_base, final int p) {
        return f0_base[p][0];
    }

    private static double y(final int[][] f0_base, final int p) {
        return f0_base[p][1];
    }

    public static void interpolate(final int[][] f0_base, int p0, final int pn, final int[] data, final double res) {
        final PointPlotter plotter = new PointPlotter(data);
        int p = p0;
        int p2 = ++p;
        int p3 = ++p2;
        ++p3;
        while (p2 != pn) {
            if (x(f0_base, p) != x(f0_base, p2)) {
                double k3;
                double k2;
                if (x(f0_base, p0) == x(f0_base, p) && x(f0_base, p2) == x(f0_base, p3)) {
                    k2 = (k3 = (y(f0_base, p2) - y(f0_base, p)) / (x(f0_base, p2) - x(f0_base, p)));
                } else if (x(f0_base, p0) == x(f0_base, p)) {
                    k2 = (y(f0_base, p3) - y(f0_base, p)) / (x(f0_base, p3) - x(f0_base, p));
                    k3 = (3.0 * (y(f0_base, p2) - y(f0_base, p)) / (x(f0_base, p2) - x(f0_base, p)) - k2) / 2.0;
                } else if (x(f0_base, p2) == x(f0_base, p3)) {
                    k3 = (y(f0_base, p2) - y(f0_base, p0)) / (x(f0_base, p2) - x(f0_base, p0));
                    k2 = (3.0 * (y(f0_base, p2) - y(f0_base, p)) / (x(f0_base, p2) - x(f0_base, p)) - k3) / 2.0;
                } else {
                    k3 = (y(f0_base, p2) - y(f0_base, p0)) / (x(f0_base, p2) - x(f0_base, p0));
                    k2 = (y(f0_base, p3) - y(f0_base, p)) / (x(f0_base, p3) - x(f0_base, p));
                }
                interpolate_forward_difference(x(f0_base, p), y(f0_base, p), x(f0_base, p2), y(f0_base, p2), k3, k2, plotter, res);
            }
            ++p0;
            ++p;
            ++p2;
            ++p3;
        }
    }

    private static class Coefficients {
        double a;
        double b;
        double c;
        double d;
    }

    public static class PointPlotter {
        private final int[] f;

        PointPlotter(final int[] arr) {
            this.f = arr;
        }

        void plot(final double x, double y) {
            if (y < 0.0) {
                y = 0.0;
            }
            this.f[(int) x] = (int) (y + 0.5);
        }
    }
}
