module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Global.Unsafe (unsafeStringify)
import Prim.Row (class Union)
import Type.Row (type (+))

type OptionalFields r = ( opt1 ∷ Boolean, opt2 ∷ Int | r )

stringify
  ∷ ∀ given missing
  . Union given missing (OptionalFields + ())
  ⇒ { | given }
  → String
stringify r =
  unsafeStringify r

logOptional ∷ Effect Unit
logOptional = do
  log $ stringify {}
  log $ stringify { opt1: true }
  log $ stringify { opt2: 2 }
  log $ stringify { opt1: false, opt2: 8 }

type RequiredFields r = ( req1 ∷ Number, req2 ∷ String | r )

stringify'
  ∷ ∀ given optionalMissing optionalPresent
  . Union (RequiredFields + ()) optionalPresent given
  ⇒ Union given optionalMissing (RequiredFields + OptionalFields + ())
  ⇒ { | given }
  → String
-- | I'm not able to use the fact from the constraint that
-- | `r` contains `req2` field.
-- | This function body won't compile:
-- | stringify' r = r.req2
-- |
-- | Of course I can provide my dummy body or pass this record to the FFI
stringify' r = unsafeStringify r

-- | Here I can use required fields inside the body because we are stating them directly
-- | in the type of the input record.
stringify''
  ∷ ∀ optionalMissing optionalPresent
  . Union optionalPresent optionalMissing (OptionalFields + ())
  ⇒ { | RequiredFields + optionalPresent }
  → String
stringify'' r = "req2:" <> r.req2 <> "; record: " <> unsafeStringify r


logOptionalAndRequired ∷ Effect Unit
logOptionalAndRequired = do
  log $ stringify' { req1: 8.0, req2: "test" }
  log $ stringify' { opt2: 2, req1: 8.0, req2: "test" }
  log $ stringify' { opt1: true, opt2: 2, req1: 8.0, req2: "test" }

  -- | This won't compile because required field req1 is miessing
  -- log $ stringify' { opt1: true, opt2: 2, req2: "test" }

  log $ stringify'' { req1: 8.0, req2: "test" }
  log $ stringify'' { opt2: 2, req1: 8.0, req2: "test" }
  log $ stringify'' { opt1: true, opt2: 2, req1: 8.0, req2: "test" }

  -- | This also won't compile because required field req1 is miessing
  -- log $ stringify'' { opt1: true, opt2: 2, req2: "test" }


main ∷ Effect Unit
main = do
  logOptional
  logOptionalAndRequired
