{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Search
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

LDAP Searching

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Search (SearchAttributes(..),
                    LDAPEntry(..), LDAPScope(..),
                    ldapSearch, 
                   )
where

import LDAP.Utils
import LDAP.Types
import LDAP.TypesLL
import LDAP.Data
import Foreign
import Foreign.C.String
#if (__GLASGOW_HASKELL__>=705)
import Foreign.C.Types(CInt(..))
#endif
import LDAP.Result
import Control.Exception(finally)

#include <ldap.h>

{- | Defines what attributes to return with the search result. -}
data SearchAttributes =
   LDAPNoAttrs                   -- ^ No attributes
 | LDAPAllUserAttrs              -- ^ User attributes only
 | LDAPAttrList [String]         -- ^ User-specified list
   deriving (Eq, Show)

sa2sl :: SearchAttributes -> [String]
sa2sl LDAPNoAttrs = [ #{const_str LDAP_NO_ATTRS} ]
sa2sl LDAPAllUserAttrs = [ #{const_str LDAP_ALL_USER_ATTRIBUTES} ]
sa2sl (LDAPAttrList x) = x

data LDAPEntry = LDAPEntry 
    {ledn :: String             -- ^ Distinguished Name of this object
    ,leattrs :: [(String, [String])] -- ^ Mapping from attribute name to values
                           }
    deriving (Eq, Show)

ldapSearch :: LDAP              -- ^ LDAP connection object
           -> Maybe String      -- ^ Base DN for search, if any
           -> LDAPScope         -- ^ Scope of the search
           -> Maybe String      -- ^ Filter to be used (none if Nothing)
           -> SearchAttributes  -- ^ Desired attributes in result set
           -> Bool              -- ^ If True, exclude attribute values (return types only)
           -> IO [LDAPEntry]

ldapSearch ld base scope filter attrs attrsonly =
  withLDAPPtr ld (\cld ->
  withMString base (\cbase ->
  withMString filter (\cfilter ->
  withCStringArr0 (sa2sl attrs) (\cattrs ->
  do msgid <- checkLEn1 "ldapSearch" ld $
              ldap_search cld cbase (fromIntegral $ fromEnum scope)
                          cfilter cattrs (fromBool attrsonly)
     procSR ld cld msgid
                               )
                      )
                    )
                  )

procSR :: LDAP -> Ptr CLDAP -> LDAPInt -> IO [LDAPEntry]
procSR ld cld msgid =
  do res1 <- ldap_1result ld msgid
     --putStrLn "Have 1result"
     withForeignPtr res1 (\cres1 ->
      do felm <- ldap_first_entry cld cres1
         if felm == nullPtr
            then return []
            else do --putStrLn "Have first entry"
                    cdn <- ldap_get_dn cld felm -- FIXME: check null
                    dn <- peekCString cdn
                    ldap_memfree cdn
                    attrs <- getattrs ld felm
                    next <- procSR ld cld msgid
                    --putStrLn $ "Next is " ++ (show next)
                    return $ (LDAPEntry {ledn = dn, leattrs = attrs}):next
                         )
      


data BerElement

getattrs :: LDAP -> (Ptr CLDAPMessage) -> IO [(String, [String])]
getattrs ld lmptr =
    withLDAPPtr ld (\cld -> alloca (f cld))
    where f cld (ptr::Ptr (Ptr BerElement)) =
              do cstr <- ldap_first_attribute cld lmptr ptr
                 if cstr == nullPtr
                    then return []
                    else do str <- peekCString cstr
                            ldap_memfree cstr
                            bptr <- peek ptr
                            values <- getvalues cld lmptr str
                            nextitems <- getnextitems cld lmptr bptr
                            return $ (str, values):nextitems

getnextitems :: Ptr CLDAP -> Ptr CLDAPMessage -> Ptr BerElement 
             -> IO [(String, [String])]
getnextitems cld lmptr bptr =
    do cstr <- ldap_next_attribute cld lmptr bptr
       if cstr == nullPtr
          then return []
          else do str <- peekCString cstr
                  ldap_memfree cstr
                  values <- getvalues cld lmptr str
                  nextitems <- getnextitems cld lmptr bptr
                  return $ (str, values):nextitems

getvalues :: LDAPPtr -> Ptr CLDAPMessage -> String -> IO [String]
getvalues cld clm attr =
    withCString attr (\cattr ->
    do berarr <- ldap_get_values_len cld clm cattr
       if berarr == nullPtr
            -- Work around bug between Fedora DS and OpenLDAP (ldapvi
            -- does the same thing)
            then return []
            else finally (procberarr berarr) (ldap_value_free_len berarr)
    )

procberarr :: Ptr (Ptr Berval) -> IO [String]
procberarr pbv =
    do bvl <- peekArray0 nullPtr pbv
       mapM bv2str bvl

foreign import ccall unsafe "ldap.h ldap_get_dn"
  ldap_get_dn :: LDAPPtr -> Ptr CLDAPMessage -> IO CString

foreign import ccall unsafe "ldap.h ldap_get_values_len"
  ldap_get_values_len :: LDAPPtr -> Ptr CLDAPMessage -> CString -> IO (Ptr (Ptr Berval))

foreign import ccall unsafe "ldap.h ldap_value_free_len"
  ldap_value_free_len :: Ptr (Ptr Berval) -> IO ()

foreign import ccall unsafe "ldap.h ldap_search"
  ldap_search :: LDAPPtr -> CString -> LDAPInt -> CString -> Ptr CString ->
                 LDAPInt -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_first_entry"
  ldap_first_entry :: LDAPPtr -> Ptr CLDAPMessage -> IO (Ptr CLDAPMessage)

foreign import ccall unsafe "ldap.h ldap_first_attribute"
  ldap_first_attribute :: LDAPPtr -> Ptr CLDAPMessage -> Ptr (Ptr BerElement) 
                       -> IO CString

foreign import ccall unsafe "ldap.h ldap_next_attribute"
  ldap_next_attribute :: LDAPPtr -> Ptr CLDAPMessage -> Ptr BerElement
                       -> IO CString
