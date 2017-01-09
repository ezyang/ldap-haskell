{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Init
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Initialization and shutdown for LDAP programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Init(ldapOpen,
                 ldapInit,
                 ldapInitialize,
                 ldapSimpleBind,
                 ldapExternalSaslBind)
where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import LDAP.Types
import Foreign.C.Types
import LDAP.Utils
import Foreign.Marshal.Utils

#include <ldap.h>


ldapSetVersion3 :: LDAPPtr -> IO LDAPInt
ldapSetVersion3 cld =
    with ((#{const LDAP_VERSION3})::LDAPInt) $ \copt ->
    ldap_set_option cld #{const LDAP_OPT_PROTOCOL_VERSION} (castPtr copt)

ldapSetRestart :: LDAPPtr -> IO LDAPInt
ldapSetRestart cld =
    with ((#{const LDAP_OPT_ON})::LDAPInt) $ \copt ->
    ldap_set_option cld #{const LDAP_OPT_RESTART} (castPtr copt)

{- | Preferred way to initialize a LDAP connection. 
The default port is given in 'LDAP.Constants.ldapPort'.

Could throw IOError on failure. -}
ldapInit :: String              -- ^ Host
         -> LDAPInt             -- ^ Port
         -> IO LDAP             -- ^ New LDAP Obj
ldapInit host port =
    withCString host $ \cs ->
       do rv <- fromLDAPPtr "ldapInit" (cldap_init cs port)
          withForeignPtr rv $ \cld -> do
              ldapSetVersion3 cld
              ldapSetRestart cld
          return rv

{- | Like 'ldapInit', but establish network connection immediately. -}
ldapOpen :: String              -- ^ Host
            -> LDAPInt          -- ^ Port
            -> IO LDAP          -- ^ New LDAP Obj
ldapOpen host port =
    withCString host (\cs ->
        do rv <- fromLDAPPtr "ldapOpen" (cldap_open cs port)
           withForeignPtr rv ldapSetRestart
           return rv)

{- | Like 'ldapInit', but accepts a URI (or whitespace/comma separated
list of URIs) which can contain a schema, a host and a port.  Besides
ldap, valid schemas are ldaps (LDAP over TLS), ldapi (LDAP over IPC),
and cldap (connectionless LDAP). -}
ldapInitialize :: String        -- ^ URI
                  -> IO LDAP    -- ^ New LDAP Obj
ldapInitialize uri =
    withCString uri $ \cs ->
    alloca $ \pp -> do
    r <- ldap_initialize pp cs
    ldap <- fromLDAPPtr "ldapInitialize" (peek pp)
    _ <- checkLE "ldapInitialize" ldap (return r)
    withForeignPtr ldap $ \p -> do
        ldapSetVersion3 p
        ldapSetRestart p
    return ldap


{- | Bind to the remote server. -}
ldapSimpleBind :: LDAP          -- ^ LDAP Object
               -> String        -- ^ DN (Distinguished Name)
               -> String        -- ^ Password
               -> IO ()
ldapSimpleBind ld dn passwd =
    withLDAPPtr ld (\ptr ->
     withCString dn (\cdn ->
      withCString passwd (\cpasswd -> 
        do checkLE "ldapSimpleBind" ld
                            (ldap_simple_bind_s ptr cdn cpasswd)
           return ()
                         )))

{- | Bind with the SASL EXTERNAL mechanism. -}
ldapExternalSaslBind :: LDAP   -- ^ LDAP Object
                     -> String -- ^ Authorization identity (UTF-8 encoded; pass "" to derive it from the authentication identity)
                     -> IO ()
ldapExternalSaslBind ld authz =
    withLDAPPtr ld (\ptr ->
     withCStringLen authz (\(authzPtr,authzLen) ->
        do checkLE "ldapExternalSaslBind" ld (external_sasl_bind ptr authzPtr authzLen)
           return ()
      ))

foreign import ccall unsafe "ldap.h ldap_init"
  cldap_init :: CString -> CInt -> IO LDAPPtr


foreign import ccall safe "ldap.h ldap_open"
  cldap_open :: CString -> CInt -> IO LDAPPtr

foreign import ccall unsafe "ldap.h ldap_initialize"
  ldap_initialize :: Ptr LDAPPtr -> CString -> IO LDAPInt

foreign import ccall safe "ldap.h ldap_simple_bind_s"
  ldap_simple_bind_s :: LDAPPtr -> CString -> CString -> IO LDAPInt

foreign import ccall safe "sasl_external.h external_sasl_bind"
  external_sasl_bind :: LDAPPtr -> CString -> Int -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_set_option"
  ldap_set_option :: LDAPPtr -> LDAPInt -> Ptr () -> IO LDAPInt
