package micro;
import systemj.lib.Signal;
public class arraytest {
  private static Signal S1;
  public static void main(String[] args) {
    arr A = null;
    arraytest at = new arraytest();
    int count = 0;
    /* Start timer */
    long t1 = System.nanoTime();
    for(count=0;count<100;++count){
      A = new arr ();
      A.testArr = new int[20];
    }
    long t2 = System.nanoTime();
    System.out.println("Time taken for array object allocation: ");
    System.out.println(t2-t1);
    S1 = new Signal();
    S1.setValue(A);
  }
}

