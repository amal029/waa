package micro;
import systemj.lib.Signal;
public class simple_object_loop {
  public static Signal S1;
  public static void main(String[] args) {
    o O = null;
    int count = 0;
    /* Start timer */
    /* long t1 = System.nanoTime(); */
    for(count=0;count<100;++count){ //@WCA loop = 100
      O = new o ();
      O.ref = new o1(count);
    }
    /* long t2 = System.nanoTime(); */
    /* System.out.println("Time taken for simple object allocation: "); */
    /* System.out.println(t2-t1); */
    S1 = new Signal();
    S1.setValue(O);
  }
}
