Obfuscate
=========

This library makes it easy to use obfuscation in your application code, allowing you to obscure numeric ids before sending them to a client (such as in a JSON API or other web service). This library is backed by [`foxhound-systems/hashids-st`](https://github.com/foxhound-systems/hashids-st), which itself is an implementation of the [Hashids.org](https://hashids.org/) obfuscation interface.

## Implementation

The `Web.Obfuscate` module provides `CanObfuscate` and `CanDeobfuscate` typeclasses along with several default instances for basic data types. In addition to manually defining your own instances, `Web.Obfuscate.TH` will derive a sensible default using `deriveObfuscate`.


## Example Usage

### Basic Usage

```haskell
import Web.Obfuscate
import Hashids

...

hashidsContext :: HashidsContext
hashidsContext = fromRight $ mkHashidsContext "mySaltGoesHere" 6 defaultAlphabet

getUser :: Obfuscated UserId -> IO (User)
getUser obfuscatedUserId =
    maybeUserId <- deobfuscate hashidsContext obfuscatedUserId
    case maybeUserId of
        Just userId ->
          fetchUserById userId
          
        Nothing ->
          throwIO err400

```

### Deriving Obfuscation Instances with Template Haskell

```haskell
{-# LANGUAGE TemplateHaskell #-}

module ForumResponse
  where

import Web.Obfuscate 
import Web.Obfuscate.TH
import qualified Data.Text as T

-- Another module that defines another custom obfuscatable type
import ForumAdministrator

data ForumResponse = ForumResponse
  { frForumId :: ForumId
  , frName :: Text
  , frDescription :: Text
  , frCreator :: ForumAdministrator
  , frAdministrators :: [ForumAdministrator]
  }

$(deriveObfuscate defaultObfuscationOptions ''ForumResponse)
```

The above Template Haskell code will generate something like the following:

```haskell
data ObfuscatedForumResponse = ObfuscatedForumResponse
   { obfrForumId :: Obfuscated ForumId
   , obfrName :: Text
   , obfrDescription :: Text
   , obfrCreator :: Obfuscated ForumAdministrator
   , obfrAdministrators :: Obfuscated [ForumAdministrator]
   }

type instance Obfuscated ForumResponse = ObfuscatedForumResponse

instance CanObfuscate ForumResponse where
  obfuscate ctx forumResponse =
    ObfuscatedForumResponse
     { obfrForumId = obfuscate ctx $ frForumId forumResponse
     , obfrName = frName forumResponse
     , obfrDescription = frDescription forumResponse
     , obfrCreator = obfuscate ctx $ frCreator forumResponse
     , obfrAdministrators = obfuscate ctx $ frAdministrators forumResponse
     }

instance CanDeobfuscateForumResponse where
  deobfuscate ctx obfuscatedForumResponse = do
    forumId <- deobfuscate ctx $ obfrForumId obfuscatedForumResponse
    creator <- deobfuscate ctx $ obfrCreator obfuscatedForumResponse
    administrators <- deobfuscate ctx $ obfrAdministrators obfuscatedForumResponse
    pure $ ForumResponse
     { frForumId = forumId
     , frName = obfrName obfuscatedForumResponse
     , frDescription = obfrDescription obfuscatedForumResponse
     , frCreator = creator
     , frAdministrators = administrators
     }
```

## Development

Development of this library is done using Nix. With `nix` installed, you can run the `nix-shell --command ghcid` command to start a ghcid session in an isolated environment. Run `nix-build` to perform a full build of the library.

## License

See the [LICENSE](https://github.com/foxhound-systems/hs-obfuscate/blob/master/LICENSE) file.
