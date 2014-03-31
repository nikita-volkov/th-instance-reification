{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import THInstanceExists.Prelude.Basic
import THInstanceExists.Prelude.TH
import THInstanceExists

main = htfMain $ htf_thisModulesTests : htf_importedTests
