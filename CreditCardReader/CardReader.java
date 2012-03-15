import java.io.Console;
import java.io.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.lang.*;

public class CardReader {
    public String CardNumber;
    public String ExpirationYear;
    public String ExpirationMonth;
    public String CardHolderName;
    private static final String CARDRegex = "^%B(.*)\\^(.*)/(.*)\\^(\\d\\d)(\\d\\d)";
    private static final Pattern Regex = Pattern.compile(CARDRegex);
 
 
    public CardReader()
    {
        CardNumber = "";
        ExpirationYear = "";
        ExpirationMonth = "";
        CardHolderName = "";
    }
    
    public void printInfo()
    {
        System.out.println("Card Number: " + CardNumber);
        System.out.println("Expiry: " + ExpirationMonth + "/" + ExpirationYear);
        System.out.println("Cardholder Name: " + CardHolderName);
    }
    
    public boolean readCard()
    {
        Console console = System.console();
        String inputData = new String (console.readPassword());
        Matcher m = Regex.matcher(inputData);
        if(m.find())
        {
            CardNumber = m.group(1).trim();
            CardHolderName = m.group(3).trim() + " " + m.group(2).trim();
            // If this causes a bug, it's not my damn problem. I'll be dead by 2100.
            ExpirationYear = "20" + m.group(4);
            ExpirationMonth =  m.group(5);
            return true;
        }
        
        return false;
    }
}