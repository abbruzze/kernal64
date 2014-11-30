package ucesoft.c64.peripheral.sid.resid2.resample;

/**
 * Return sample with linear interpolation.
 * 
 * @author Antti Lankila
 */
public class ZeroOrderResampler implements Resampler {
	private int cachedSample;
	
	private final int cyclesPerSample;
	private int sampleOffset;
	private int output;
	
	public ZeroOrderResampler(double clockFrequency, double samplingFrequency) {
		cyclesPerSample = (int) (clockFrequency / samplingFrequency * 1024f);
	}
	
	public boolean input(int sample) {
		boolean ready = false;

		if (sampleOffset < 1024) {
			output = cachedSample + (sampleOffset * (sample - cachedSample) >> 10);
			ready = true;
			sampleOffset += cyclesPerSample;
		}
		sampleOffset -= 1024;

		cachedSample = sample;
		
		return ready;
	}

	public int output() {
		return output;
	}
	
	public void reset() {
		sampleOffset = 0;
		cachedSample = 0;
	}
}
