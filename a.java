package a;
public class a {
	public static void main (String [] args)
	{
		int a = 0;
		for(int i=0;i<10;i++){ // @WCA loop = 20
			a++; 
		}
		for(int i=0;i<10;i++){ // @WCA loop = 300
			a++; 
		}
		tmp();
		a--;
	}
	public static void tmp (){
		int g = 2;
		g++;
	}

}
