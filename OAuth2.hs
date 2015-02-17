module OAuth2
    where

import URI
import CSRFToken
import Data.Monoid ((<>))
import Data.List (intercalate)
import Network.HTTP.Base (urlEncode)

data OAuth2WebServerFlow = OAuth2WebServerFlow
    { token :: !CSRFToken,
      clientSecret :: !String,
      scope :: ![String],
      redirectUri :: !String,
      userAgent :: !String,
      authUri :: !String,
      tokenUri :: !String,
      revokeUri :: !String,
      loginHint :: !String,
      deviceUri :: !String
    }

get :: OAuth2WebServerFlow -> String
get flow = request flow

instance Show OAuth2WebServerFlow
    where
      show oauth = show $ clientId $ token oauth

instance URI OAuth2WebServerFlow
    where
      render flow = authUri flow
                    <>"?scope=" <> intercalate "+" (scope flow)
                    <>"&redirect_uri="<>redirectUri flow
                    <>"&response_type=code"
                    <>"&client_id="<>clientId (token flow)



