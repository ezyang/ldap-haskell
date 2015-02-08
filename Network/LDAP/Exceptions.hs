{-# LANGUAGE DeriveDataTypeable #-}
{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005-2009 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Exceptions
   Copyright  : Copyright (C) 2005-2009 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Handling LDAP Exceptions

Written by John Goerzen, jgoerzen\@complete.org
-}

module Network.LDAP.Exceptions
( LDAPException(..) -- * Types
, catchLDAP         -- * General Catching
, handleLDAP
, failLDAP
, throwLDAP
) where

import Data.Typeable
import Control.Exception
import Network.LDAP.Types
import Network.LDAP.Data

#if __GLASGOW_HASKELL__ < 610
import Data.Dynamic
#endif

{- | The basic type of LDAP exceptions.  These are raised when an operation
does not indicate success. -}

data LDAPException = LDAPException 
    {code :: LDAPReturnCode,     -- ^ Numeric error code
     description :: String,     -- ^ Description of error
     caller :: String           -- ^ Calling function
    }
    deriving (Typeable)
instance Show LDAPException where
    show x = caller x ++ ": LDAPException " ++ show (code x) ++ 
             "(" ++ show (fromEnum $ code x) ++ "): " ++
             description x

instance Eq LDAPException where
    x == y = code x == code y

instance Ord LDAPException where
    compare x y = compare (code x) (code y)

#if __GLASGOW_HASKELL__ >= 610
instance Exception LDAPException where
{-
    toException = SomeException
    fromException (SomeException e) = Just e
    fromException _ = Nothing
-}

{- | Execute the given IO action.

If it raises a 'LDAPException', then execute the supplied handler and return
its return value.  Otherwise, process as normal. -}
catchLDAP :: IO a -> (LDAPException -> IO a) -> IO a
catchLDAP action handler = 
    catchJust ldapExceptions action handler

{- | Like 'catchLDAP', with the order of arguments reversed. -}
handleLDAP :: (LDAPException -> IO a) -> IO a -> IO a
handleLDAP = flip catchLDAP

{- | Given an Exception, return Just LDAPException if it was an
'LDAPExcetion', or Nothing otherwise.  Useful with functions
like catchJust. -}
ldapExceptions :: LDAPException -> Maybe LDAPException
ldapExceptions e = Just e

#else

{- | Execute the given IO action.

If it raises a 'LDAPException', then execute the supplied handler and return
its return value.  Otherwise, process as normal. -}
catchLDAP :: IO a -> (LDAPException -> IO a) -> IO a
catchLDAP = catchDyn

{- | Like 'catchLDAP', with the order of arguments reversed. -}
handleLDAP :: (LDAPException -> IO a) -> IO a -> IO a
handleLDAP = flip catchLDAP

#endif

{- | Catches LDAP errors, and re-raises them as IO errors with fail.
Useful if you don't care to catch LDAP errors, but want to see a sane
error message if one happens.  One would often use this as a high-level
wrapper around LDAP calls.
-}
failLDAP :: IO a -> IO a
failLDAP action =
    catchLDAP action handler
    where handler e = fail ("LDAP error: " ++ show e)

{- | A utility function to throw an 'LDAPException'.  The mechanics of throwing
such a thing differ between GHC 6.8.x, Hugs, and GHC 6.10.  This function
takes care of the special cases to make it simpler.

With GHC 6.10, it is a type-restricted alias for throw.  On all other systems,
it is a type-restricted alias for throwDyn. -}
throwLDAP :: LDAPException -> IO a
#if __GLASGOW_HASKELL__ >= 610
throwLDAP = throw
#else
throwLDAP = throwDyn
#endif
