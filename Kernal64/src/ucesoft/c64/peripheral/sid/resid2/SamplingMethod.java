package ucesoft.c64.peripheral.sid.resid2;

public enum SamplingMethod {
	DECIMATE("Decimate"), RESAMPLE("Resample");

	final String description;

	SamplingMethod(String description) {
		this.description = description;
	}

	@Override
	public String toString() {
		return description;
	}
}
