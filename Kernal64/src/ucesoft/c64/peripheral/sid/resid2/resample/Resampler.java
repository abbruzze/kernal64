package ucesoft.c64.peripheral.sid.resid2.resample;

/**
 * Abstraction of a resampling process. Given enough input, produces output.
 * Constructors take additional arguments that configure these objects.
 * 
 * @author Antti Lankila
 */
public interface Resampler {
	/**
	 * Input a sample into resampler. Output "true" when resampler is ready with new sample.
	 * 
	 * @param sample The sample to input into the resampler.
	 * @return true when a sample is ready
	 */
	boolean input(int sample);
	
	/**
	 * Output a sample from resampler
	 * 
	 * @return resampled sample
	 */
	int output();

	/**
	 * Resets this resampler.
	 */
	void reset();
}
