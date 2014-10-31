package micro;
import systemj.lib.Signal;
public class arraytest {
  private static Signal S1;
  public static void main(String[] args) {
    arr A = null;
    arraytest at = new arraytest();
    int count = 0;
    /* Start timer */
    int t1 = (int)System.nanoTime();
    for(count=0;count<4000;++count){
      A = new arr ();
      A.testArr = new int[20];
    }
    int t2 = (int)System.nanoTime();
    System.out.println(t2-t1);
    System.out.println("Time taken for array object allocation: ");
    S1 = new Signal();
    S1.setValue(A);
  }
}

