package micro;
import systemj.lib.Signal;
public class complex_object_test {
  public static Signal S1;
  public static void main(String[] args) {
    o[] objects = null; 
    int count = 0;
    /* Start timer */
    long t1 = System.nanoTime();
    for(int i=0;i<200;++i){
      objects = new o[20];
      for(int j=0;j<20;++j){
	o newObject = new o();
	newObject.ref = new o1(j);
	objects[j] = newObject;
      }
    } 
    long t2 = System.nanoTime();
    System.out.println("Time taken for complex object allocation: ");
    System.out.println(t2-t1);
    S1 = new Signal();
    S1.setValue(objects);
  }
}
