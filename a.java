package a;
public class a {
	public static void main (String [] args)
	{
		int a = 0;
		for(int i=0;i<10;i++){ // @WCA loop = 20
			a++; 
		}
		while(true){// @WCA loop = 300
			{
				a++;
				a--;
				a++;
				tmp();
				a--;
			}
		}
	}
	public static void tmp (){
		int g = 2;
		g++;
	}

}
