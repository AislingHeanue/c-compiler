int main(void) {
	static double zero = 0.0;
	double nan = 0.0 / zero; // make this constant-folding proof
	if (nan == 0.0) {
		return 1;
	}
	if (nan > 0.0) {
		return 3;
	}
	if (nan < 0.0) {
		return 2;
	}
	if (nan <= 0.0) {
		return 4;
	}
	if (nan >= 0.0) {
		return 5;
	}
	if (nan < 0.0 || nan == 0.0 || nan > 0.0 || nan <= 0.0 || nan >= 0.0)
		return 6;
	return 0;
}
