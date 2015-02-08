{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Modify
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

LDAP changes

Written by John Goerzen, jgoerzen\@complete.org
-}

module Network.LDAP.Modify 
( ModOp(..)
, Mod(..)
, add
, modify
, delete
, list2ldm
, pairs2ldm
) where

import Network.LDAP.Utils
import Network.LDAP.Types
import Network.LDAP.TypesLL
import Network.LDAP.Data
import Foreign
import Foreign.C.String
#if (__GLASGOW_HASKELL__>=705)
import Foreign.C.Types(CInt(..))
#endif
import Network.LDAP.Result
import Control.Exception(finally)
import Data.Bits

#include <ldap.h>

data Mod = Mod { modOp   :: ModOp -- ^ Type of operation to perform
               , modType :: String    -- ^ Name of attribute to edit
               , modVals :: [String]  -- ^ New values
               } deriving (Eq, Show)

modify :: LDAP     -- ^ LDAP connection object
       -> String   -- ^ DN to modify
       -> [Mod]    -- ^ Changes to make
       -> IO ()
modify = genericChange "ldapModify" ldap_modify_s

add :: LDAP                 -- ^ LDAP connection object
    -> String               -- ^ DN to add
    -> [Mod]            -- ^ Items to add
    -> IO ()
add = genericChange "ldapAdd" ldap_add_s

genericChange name func ld dn changelist =
    withLDAPPtr ld (\cld ->
    withCString dn (\cdn ->
    withCModArr0 changelist (\cmods ->
    do checkLE name ld $ func cld cdn cmods
       return ()
            )))

{- | Delete the specified DN -}
delete :: LDAP -> String -> IO ()
delete ld dn =
    withLDAPPtr ld (\cld ->
    withCString dn (\cdn ->
    do checkLE "ldapDelete" ld $ ldap_delete_s cld cdn
       return ()
                   ))

{- | Takes a list of name\/value points and converts them to Mod 
entries.  Each item will have the specified 'LDAPModOp'. -}
list2ldm :: ModOp -> [(String, [String])] -> [Mod]
list2ldm mo namevals = map modFromNameValue namevals where
        modFromNameValue (key, vals) = Mod { modOp   = mo
                                           , modType = key
                                           , modVals = vals
                                           }

{- | Similar to list2ldm, but handles pairs with only one value. -}
pairs2ldm :: ModOp -> [(String, String)] -> [Mod]
pairs2ldm mo = list2ldm mo . map (\(x, y) -> (x, [y]))

data CMod

newCMod :: Mod -> IO (Ptr CMod)
newCMod lm =
    do (ptr::(Ptr CMod)) <- mallocBytes #{size LDAPMod}
       cmodtype <- newCString (modType lm)
       let (cmodop::LDAPInt) = 
               (fromIntegral . fromEnum . modOp $ lm) .|. 
               #{const LDAP_MOD_BVALUES}
       bervals <- mapM newBerval (modVals lm)
       (arrptr::Ptr (Ptr Berval)) <- newArray0 nullPtr bervals 
       ( #{ poke LDAPMod, mod_op} ) ptr cmodop
       ( #{ poke LDAPMod, mod_type } ) ptr cmodtype
       ( #{ poke LDAPMod, mod_vals } ) ptr arrptr
       return ptr

freeCMod :: Ptr CMod -> IO ()
freeCMod ptr =
    do -- Free the array of Bervals
       (arrptr::Ptr (Ptr Berval)) <- ( #{ peek LDAPMod
                                        , mod_vals
                                        }
                                     ) ptr
       (arr::[Ptr Berval]) <- peekArray0 nullPtr arrptr
       mapM_ freeHSBerval arr
       free arrptr
       -- Free the modtype
       (cmodtype::CString) <- ( #{ peek LDAPMod
                                 , mod_type
                                 }
                              ) ptr
       free cmodtype
       -- mod_op is an int and doesn't need freeing
       -- free the LDAPMod itself.
       free ptr
       
withCModArr0 :: [Mod] -> (Ptr (Ptr CMod) -> IO a) -> IO a
withCModArr0 = withAnyArr0 newCMod freeCMod

foreign import ccall unsafe "ldap.h ldap_modify_s"
  ldap_modify_s :: LDAPPtr -> CString -> Ptr (Ptr CMod) -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_delete_s"
  ldap_delete_s :: LDAPPtr -> CString -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_add_s"
  ldap_add_s :: LDAPPtr -> CString -> Ptr (Ptr CMod) -> IO LDAPInt
