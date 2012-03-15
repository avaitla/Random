//http://devlog.bafford.us/two-legged-oauth-in-java-using-scribe-to-acce
import org.scribe.model.*;

public static class Transactions extends DefaultApi10a {
    @Override
    public String getAccessTokenEndpoint ()       { return ""; };
    
    @Override
    public String getRequestTokenEndpoint()       { return ""; };
    
    @Override
    public String getAuthorizationUrl(Token arg0) { return ""; };    
}