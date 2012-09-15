{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Result
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

LDAP Result Processing

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Result (LDAPMessage, CLDAPMessage,
                    ldap_1result
                   ) where

import LDAP.Utils
import LDAP.Types
import Foreign
#if (__GLASGOW_HASKELL__>=705)
import Foreign.C.Types(CInt(..))
#endif

#include <ldap.h>

data CLDAPMessage
type LDAPMessage = ForeignPtr CLDAPMessage

{- | Get 1 result from an operation. -}
ldap_1result :: LDAP -> LDAPInt -> IO (LDAPMessage)
ldap_1result ld msgid =
    withLDAPPtr ld (\cld ->
    alloca (f cld)
    )
    where f cld (ptr::Ptr (Ptr CLDAPMessage)) =
              do checkLEn1 "ldap_1result" ld $
                         ldap_result cld msgid 0 nullPtr ptr
                 fromldmptr "ldap_1result" (peek ptr)

fromldmptr :: String -> IO (Ptr CLDAPMessage) -> IO LDAPMessage
fromldmptr caller action =
    do ptr <- action
       if ptr == nullPtr
          then fail (caller ++ ": got null LDAPMessage pointer")
          else newForeignPtr ldap_msgfree_call ptr

foreign import ccall unsafe "ldap.h ldap_result"
  ldap_result :: LDAPPtr -> LDAPInt -> LDAPInt -> Ptr () -> Ptr (Ptr CLDAPMessage) -> IO LDAPInt

foreign import ccall unsafe "ldap.h &ldap_msgfree"
  ldap_msgfree_call :: FunPtr (Ptr CLDAPMessage -> IO ())
