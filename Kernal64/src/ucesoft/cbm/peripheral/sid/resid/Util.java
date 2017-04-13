package ucesoft.cbm.peripheral.sid.resid;

public class Util {
  public static int[] unpack(int[] data) {
    int[] tmp = new int[4096];
    int pos = 0;
    for (int i = 0; i < data.length; i++) {
      int d = data[i];
      if ((d >> 8) > 0) {
        int loopMax = d >> 8;
        for (int j = 0; j < loopMax; j++)
          tmp[pos++] = d & 0xff;
      } else {
        tmp[pos++] = d;
      }
    }
    //System.out.println("Upacked " + data.length + " to " + pos);
    return tmp;
  }
}
