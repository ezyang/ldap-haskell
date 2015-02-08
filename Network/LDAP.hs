{- -*- Mode: haskell; -*-
Haskell LDAP Interface

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP
   Copyright  : Copyright (C) 2005-2007 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Top-level LDAP module.

Written by John Goerzen, jgoerzen\@complete.org

Welcome to the LDAP interface for Haskell.  Please see one of the sections
below for more information.

This package comes from:

<http://software.complete.org/ldap-haskell>
-}

module Network.LDAP
    ( module Network.LDAP.Types      -- * Basic Types
    , module Network.LDAP.Init       -- * Initialization
    , module Network.LDAP.Search     -- * Searching
    , module Network.LDAP.Modify     -- * Adding, Deleting, and Altering,
    , module Network.LDAP.Exceptions -- * Error Handling
    , module Network.LDAP.Data       -- * Haskell enumerated LDAP types
    , module Network.LDAP.Constants  -- * Other LDAP constants
    )
where
import Network.LDAP.Exceptions
import Network.LDAP.Types
import Network.LDAP.Init
import Network.LDAP.Data
import Network.LDAP.Constants
import Network.LDAP.Search hiding (LDAPScope(..))
import Network.LDAP.Modify hiding (LDAPModOp(..))

