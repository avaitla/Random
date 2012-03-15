public class Temporary
{ 
	public static void main(String [] args)
	{
		CardReader card = new CardReader();
		if(card.readCard()) card.printInfo();
		else System.out.println("Card Read Failure!");
	}
}