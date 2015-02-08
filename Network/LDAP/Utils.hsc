{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Utils
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

LDAP low-level utilities

Written by John Goerzen, jgoerzen\@complete.org

Please use sparingly and with caution.  The documentation for their behavior
should be considered to be the source code.

-}

module Network.LDAP.Utils
( checkLE
, checkLEe
, checkLEn1
, checkNULL
, LDAPPtr
, fromLDAPPtr
, withLDAPPtr
, maybeWithLDAPPtr
, withMString
, withCStringArr0
, ldap_memfree
, bv2str
, newBerval
, freeHSBerval
, withAnyArr0
) where

import Foreign.Ptr
import Network.LDAP.Constants
import Network.LDAP.Exceptions
import Network.LDAP.Types
import Network.LDAP.Data
import Network.LDAP.TypesLL
import Control.Exception
import Data.Dynamic
import Foreign.C.Error
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign
import Foreign.C.Types

#include <ldap.h>

{- FIXME frmo python: 

   return native oom for LDAP_NO_MEMORY?
   load up LDAP_OPT_MATCHED_DN?
   handle LDAP_REFERRAL?
   -}

{- | Check the return value.  If it's something other than 
'LDAP.Constants.ldapSuccess', raise an LDAP exception. -}
checkLE :: String -> LDAP -> IO LDAPInt -> IO LDAPInt
checkLE = checkLEe (\r -> r == fromIntegral (fromEnum Success))

checkLEn1 :: String -> LDAP -> IO LDAPInt -> IO LDAPInt
checkLEn1 = checkLEe (\r -> r /= -1)

checkLEe :: (LDAPInt -> Bool) -> String -> LDAP -> IO LDAPInt -> IO LDAPInt
checkLEe test callername ld action =
    do result <- action
       if test result
          then return result
          else do errornum <- ldapGetOptionIntNoEc ld OptErrorNumber
                  let hserror = toEnum (fromIntegral errornum)
                  err2string <- (ldap_err2string errornum >>= peekCString)
                  objstring <- ldapGetOptionStrNoEc ld OptErrorString
                  let desc = case objstring of
                                             Nothing -> err2string
                                             Just x -> err2string ++ " (" ++
                                                       x ++ ")"
                  let exc = LDAPException {code = hserror,
                                           description = desc,
                                           caller = callername }
                  throwLDAP exc

{- | Raise an IOError based on errno if getting a NULL.  Identical
to Foreign.C.Error.throwErrnoIfNull. -}
checkNULL :: String -> IO (Ptr a) -> IO (Ptr a)
checkNULL = throwErrnoIfNull

{- | Value coming in from C -}
type LDAPPtr = Ptr CLDAP

{- | Convert a LDAPPtr into a LDAP type.  Checks it with 'checkNULL'
automatically. -}
fromLDAPPtr :: String -> IO LDAPPtr -> IO LDAP
fromLDAPPtr caller action =
    do ptr <- checkNULL caller action
       newForeignPtr ldap_unbind ptr

{- | Use a 'LDAP' in a function that needs 'LDAPPtr'. -}
withLDAPPtr :: LDAP -> (LDAPPtr -> IO a) -> IO a
withLDAPPtr ld = withForeignPtr ld

{- | Same as 'withLDAPPtr', but uses nullPtr if the input is Nothing. -}
maybeWithLDAPPtr :: Maybe LDAP -> (LDAPPtr -> IO a) -> IO a
maybeWithLDAPPtr Nothing func = func nullPtr
maybeWithLDAPPtr (Just x) y = withLDAPPtr x y

{- | Returns an int, doesn't raise exceptions on err (just crashes) -}
ldapGetOptionIntNoEc :: LDAP -> OptionCode -> IO LDAPInt
ldapGetOptionIntNoEc ld oc =
    withLDAPPtr ld (\pld -> alloca (f pld))
    where oci = fromIntegral $ fromEnum oc
          f pld (ptr::Ptr LDAPInt) =
              do res <- ldap_get_option pld oci (castPtr ptr)
                 if res /= 0
                    then fail $ "Crash in int ldap_get_option, code " ++ show res
                    else peek ptr

{- | Returns a string, doesn't raise exceptions on err (just crashes) -}
ldapGetOptionStrNoEc :: LDAP -> OptionCode -> IO (Maybe String)
ldapGetOptionStrNoEc ld oc =
    withLDAPPtr ld (\pld -> alloca (f pld))
    where
    oci = fromEnum oc
    f pld (ptr::Ptr CString) = 
        do res <- ldap_get_option pld (fromIntegral oci) (castPtr ptr)
           if res /= 0
              then fail $ "Crash in str ldap_get_option, code " ++ show res
              else do cstr <- peek ptr
                      fp <- wrap_memfree cstr
                      withForeignPtr fp (\cs ->
                       do if cs == nullPtr
                             then return Nothing
                             else do hstr <- peekCString cs
                                     return $ Just hstr
                                        )

wrap_memfree :: CString -> IO (ForeignPtr Foreign.C.Types.CChar)
wrap_memfree p = newForeignPtr ldap_memfree_call p

withMString :: Maybe String -> (CString -> IO a) -> IO a
withMString Nothing action = action (nullPtr)
withMString (Just str) action = withCString str action

withCStringArr0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArr0 inp action = withAnyArr0 newCString free inp action

withAnyArr0 :: (a -> IO (Ptr b)) -- ^ Function that transforms input data into pointer
            -> (Ptr b -> IO ())  -- ^ Function that frees generated data
            -> [a]               -- ^ List of input data
            -> (Ptr (Ptr b) -> IO c) -- ^ Action to run with the C array
            -> IO c             -- Return value
withAnyArr0 input2ptract freeact inp action =
    bracket (mapM input2ptract inp)
            (\clist -> mapM_ freeact clist)
            (\clist -> withArray0 nullPtr clist action)

withBervalArr0 :: [String] -> (Ptr (Ptr Berval) -> IO a) -> IO a
withBervalArr0 = withAnyArr0 newBerval freeHSBerval

bv2str :: Ptr Berval -> IO String
bv2str bptr = 
    do (len::BERLen) <- ( #{peek struct berval, bv_len} ) bptr
       cstr <- ( #{peek struct berval, bv_val} ) bptr
       peekCStringLen (cstr, fromIntegral len)

{- | Must be freed later with freeHSBerval! -}

newBerval :: String -> IO (Ptr Berval)
newBerval str =
           do (ptr::Ptr Berval) <- mallocBytes #{size struct berval}
              (cstr, len) <- newCStringLen str
              let (clen::BERLen) = fromIntegral len
              ( #{poke struct berval, bv_len} ) ptr clen
              ( #{poke struct berval, bv_val} ) ptr cstr
              return ptr

{- | Free a berval allocated from Haskell. -}
freeHSBerval :: Ptr Berval -> IO ()
freeHSBerval ptr =
    do cstr <- ( #{peek struct berval, bv_val} ) ptr
       free cstr
       free ptr

foreign import ccall unsafe "ldap.h &ldap_unbind"
  ldap_unbind :: FunPtr (LDAPPtr -> IO ()) -- ldap_unbind, ignoring retval

foreign import ccall unsafe "ldap.h ldap_err2string"
  ldap_err2string :: LDAPInt -> IO CString

foreign import ccall unsafe "ldap.h ldap_get_option"
  ldap_get_option :: LDAPPtr -> LDAPInt -> Ptr () -> IO LDAPInt

foreign import ccall unsafe "ldap.h &ldap_memfree"
  ldap_memfree_call :: FunPtr (CString -> IO ())

foreign import ccall unsafe "ldap.h ldap_memfree"
  ldap_memfree :: CString -> IO ()
