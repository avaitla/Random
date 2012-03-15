import org.scribe.builder.*;
import org.scribe.builder.api.*;
import org.scribe.model.*;
import org.scribe.oauth.*;

public class TwoLeggedAuth {
    private static final String PROTECTED_RESOURCE_URL_Primary = "https://207.291.12.101";
    private static final String PROTECTED_RESOURCE_URL_Backup1 = "https://207.291.12.100";
    private static final String PROTECTED_RESOURCE_URL_Backup2 = "https://207.291.12.99";
    
    String status;
    
    public TwoLeggedAuth(String api_key, String secret, String body)
    {
        OAuthService service = new ServiceBuilder()
                                     .provider(TwoLeggedOAuth.class)
                                     .apiKey(api_key);
                                     .apiSecret(secret)
                                     .build();
        
        // for 3-legged you would need to request the authorization token         
        // OpenGeo is a two-legged OAuth server, so the token is empty         
        Token token = new Token("", "");            
        OAuthRequest request = new OAuthRequest(Verb.GET, PROTECTED_RESOURCE_URL);
        request.addPayload(body);
        service.signRequest(token, request);         
        Response response = request.send();            
        status = response.getBody();
    }    
}