{-  -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Types
   Copyright  : Copyright (C) 2005-2006 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Basic types for LDAP programs.

Written by John Goerzen, jgoerzen\@complete.org

See also "Network.LDAP.Data" for types relating to return codes,
option codes, etc.
-}

module Network.LDAP.Types
( LDAP
, LDAPInt
, BERInt
, BERTag
, BERLen
) where

import Foreign.Ptr
import Data.Word
import Data.Int
import Foreign.C.Types
import Foreign.ForeignPtr
import Network.LDAP.TypesLL
import Network.LDAP.Data

#include <ldap.h>

{- | Main LDAP object type.

LDAP objects are automatically unbound (and memory freed) when they are
garbage-collected by Haskell. -}
type LDAP = ForeignPtr CLDAP

{- | Convenience type so we use the correct ints for the LDAP library. -}
type LDAPInt = CInt

{- | BER type tag -}
type BERTag = #type ber_tag_t

{- | BER int type -}
type BERInt = #type ber_int_t

{- | BER length type -}
type BERLen = #type ber_len_t
