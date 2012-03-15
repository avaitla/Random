public class Temporary
{ 
	public static void main(String [] args)
	{
		CardReader card = new CardReader();
		if(card.readCard())
		{
		    card.printInfo();
		    TwoLeggedAuth auth = new TwoLeggedAuth("my_key", "my_password", jsonifyCard(card, 44.10));
	        System.out.println("Status: " + auth.status);
	    }
		else System.out.println("Card Read Failure!");
	}
	
	public String jsonify(string CardNumber, string ExpirationMonth,
	                      string ExpirationYear, string CardHolderName, 
	                      double chargeAmount)
    {
        JSONObject json = new JSONObject();
        json.put("cardNumber", CardNumber);
        json.put("amount", chargeAmount);
        json.put("expiryMonth", ExpirationMonth);
        json.put("expiryYear", ExpirationYear);
        json.put("cardholderName", CardHolderName);
        return json.toString();
    }
    
    public String jsonifyCard(CardReader card, double amount)
    {
        return jsonify(card.CardNumber, card.ExpirationMonth, card.ExpirationYear, card.CardHolderName, amount);
    }
}