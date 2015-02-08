{- arch-tag: Tests main file
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

module Tests(tests) where
import Test.HUnit
import Control.Exception
import Network.LDAP

testSearchNoAttrs = TestCase $ do
    -- A sample public LDAP server
    ldap <- initialize "ldap://scripts.mit.edu/"
    r <- search ldap (Just "ou=People,dc=scripts,dc=mit,dc=edu") OneLevel Nothing NoAttrs True
    assertBool "Search does not return results" (length r > 0)

tests = TestList [TestLabel "testSearchNoAttrs" testSearchNoAttrs
                 ]
