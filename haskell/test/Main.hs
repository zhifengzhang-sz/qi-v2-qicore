{-# LANGUAGE OverloadedStrings #-}

-- | QiCore Base Component Test Suite - 2025 Edition
-- 
-- Modern Haskell property-based testing using Tasty + QuickCheck
-- Following behavioral contracts with comprehensive mathematical law verification.
module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Qi.Base.Result as R
import Qi.Base.Error as E
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..))

-- | Main test entry point - modern Tasty-based test suite
main :: IO ()
main = defaultMain $ testGroup "QiCore Base Component Tests"
  [ testGroup "Mathematical Law Verification"
    [ functorLawTests
    , applicativeLawTests  
    , monadLawTests
    ]
  , testGroup "Result Contract Compliance"
    [ factoryOperationTests
    , queryPropertyTests
    , transformationOperationTests
    , extractionOperationTests
    ]
  , testGroup "QiError Contract Compliance"
    [ errorQueryTests
    , errorTransformationTests
    ]
  ]

-- ============================================================================
-- Modern QuickCheck Generators (2025 Best Practices)
-- ============================================================================

-- | Generate arbitrary Text values for testing
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

-- | Generate arbitrary ErrorCategory values
instance Arbitrary ErrorCategory where
  arbitrary = arbitraryBoundedEnum

-- | Generate arbitrary ErrorSeverity values  
instance Arbitrary ErrorSeverity where
  arbitrary = arbitraryBoundedEnum

-- | Generate arbitrary QiError values with proper timestamp handling
instance Arbitrary QiError where
  arbitrary = do
    code <- arbitrary
    message <- arbitrary
    category <- arbitrary
    severity <- arbitrary
    -- Use epoch time for deterministic testing
    let timestamp = read "1970-01-01 00:00:00 UTC"
    let context = Map.empty
    let cause = Nothing -- Avoid infinite nesting in property tests
    Prelude.pure $ QiError code message category context cause timestamp severity

-- ============================================================================
-- Mathematical Law Tests (Contract Requirements)
-- ============================================================================

-- | Test Functor laws for Result<T>
functorLawTests :: TestTree
functorLawTests = testGroup "Functor Laws"
  [ QC.testProperty "Identity Law: fmap id == id" $ \(r :: R.Result Int) ->
      R.map id r === r
      
  , QC.testProperty "Composition Law: fmap (f . g) == fmap f . fmap g" $ 
      \(r :: R.Result Int) ->
        let f = (+1)
            g = (*2)
        in R.map (f . g) r === (R.map f . R.map g) r
  ]

-- | Test Applicative laws for Result<T>
applicativeLawTests :: TestTree
applicativeLawTests = testGroup "Applicative Laws"
  [ QC.testProperty "Identity: apply(pure(id))(v) == v" $ \(r :: R.Result Int) ->
      R.apply (R.pure id) r === r
      
  , QC.testProperty "Homomorphism: apply(pure(f))(pure(x)) == pure(f(x))" $ \(x :: Int) ->
      let f = (+1)
      in R.apply (R.pure f) (R.pure x) === R.pure (f x)
  ]

-- | Test Monad laws for Result<T>
monadLawTests :: TestTree
monadLawTests = testGroup "Monad Laws"
  [ QC.testProperty "Left Identity: flatMap(f)(pure(a)) == f(a)" $ \(a :: Int) ->
      let f x = R.pure (x + 1) -- Simple pure function
      in R.flatMap f (R.pure a) === f a
        
  , QC.testProperty "Right Identity: result.flatMap(pure) == result" $ \(r :: R.Result Int) ->
      R.flatMap R.pure r === r
      
  , QC.testProperty "Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))" $
      \(r :: R.Result Int) ->
        let f x = R.pure (x + 1)
            g x = R.pure (x * 2)
        in R.flatMap g (R.flatMap f r) === R.flatMap (\x -> R.flatMap g (f x)) r
  ]

-- ============================================================================
-- Result Contract Compliance Tests
-- ============================================================================

-- | Test factory operations according to contracts
factoryOperationTests :: TestTree
factoryOperationTests = testGroup "Factory Operations"
  [ QC.testProperty "success(x).isSuccess() == true" $ \(x :: Int) ->
      R.isSuccess (R.success x) === True
      
  , QC.testProperty "failure(e).isFailure() == true" $ \e ->
      R.isFailure (R.failure e) === True
      
  , QC.testProperty "fromMaybe Nothing produces failure" $ \e ->
      R.isFailure (R.fromMaybe e (Nothing :: Maybe Int)) === True
      
  , QC.testProperty "fromMaybe (Just x) produces success" $ \(x :: Int) e ->
      R.getValue (R.fromMaybe e (Just x)) === Just x
  ]

-- | Test query properties according to contracts
queryPropertyTests :: TestTree
queryPropertyTests = testGroup "Query Properties"
  [ QC.testProperty "isSuccess XOR isFailure" $ \(r :: R.Result Int) ->
      R.isSuccess r /= R.isFailure r
      
  , QC.testProperty "getValue consistency with isSuccess" $ \(r :: R.Result Int) ->
      R.isSuccess r === case R.getValue r of
        Just _ -> True
        Nothing -> False
        
  , QC.testProperty "getError consistency with isFailure" $ \(r :: R.Result Int) ->
      R.isFailure r === case R.getError r of
        Just _ -> True
        Nothing -> False
  ]

-- | Test transformation operations according to contracts
transformationOperationTests :: TestTree
transformationOperationTests = testGroup "Transformation Operations"
  [ QC.testProperty "map on Success applies function" $ \(x :: Int) ->
      R.map (+1) (R.success x) === R.success (x + 1)
      
  , QC.testProperty "map on Failure preserves error" $ \e ->
      R.map (+1) (R.failure e :: R.Result Int) === R.failure e
      
  , QC.testProperty "mapError on Success preserves value" $ \(x :: Int) ->
      R.mapError (E.withSeverity E.HIGH) (R.success x) === R.success x
      
  , QC.testProperty "andThen == flip flatMap" $ \(r :: R.Result Int) ->
      let f x = R.success (x * 2)
      in R.andThen r f === R.flatMap f r
      
  , QC.testProperty "collect flattens nested Results" $ \(x :: Int) ->
      R.collect (R.success (R.success x)) === R.success x
  ]

-- | Test extraction operations according to contracts
extractionOperationTests :: TestTree
extractionOperationTests = testGroup "Extraction Operations"
  [ QC.testProperty "unwrapOr with Success returns value" $ \(x :: Int) (def :: Int) ->
      R.unwrapOr def (R.success x) === x
      
  , QC.testProperty "unwrapOr with Failure returns default" $ \e (def :: Int) ->
      R.unwrapOr def (R.failure e :: R.Result Int) === def
      
  , QC.testProperty "match handles both cases correctly" $ \(r :: R.Result Int) ->
      let onSuccess x = x + 1
          onError _ = -1
          result = R.match onSuccess onError r
      in case r of
           R.Success x -> result === x + 1
           R.Failure _ -> result === -1
  ]

-- ============================================================================
-- QiError Contract Compliance Tests
-- ============================================================================

-- | Test error query operations according to contracts
errorQueryTests :: TestTree
errorQueryTests = testGroup "Error Query Operations"
  [ QC.testProperty "getCategory returns correct category" $ \err ->
      E.getCategory err === E.qiErrorCategory err
      
  , QC.testProperty "toString produces non-empty output" $ \err ->
      not (T.null (E.toString err))
      
  , QC.testProperty "toStructuredData has all required fields" $ \err ->
      let structured = E.toStructuredData err
      in Map.size structured >= 6 -- At least 6 required fields per contract
  ]

-- | Test error transformation operations according to contracts
errorTransformationTests :: TestTree
errorTransformationTests = testGroup "Error Transformation Operations"
  [ QC.testProperty "withContext merges new context correctly" $ \err ->
      let newContext = Map.fromList [("key", String "value")]
          updated = E.withContext newContext err
      in Map.lookup "key" (E.qiErrorContext updated) === Just (String "value")
      
  , QC.testProperty "withCause sets cause correctly" $ \err1 err2 ->
      let updated = E.withCause err1 err2
      in E.qiErrorCause updated === Just err1
      
  , QC.testProperty "withSeverity updates severity correctly" $ \err ->
      let updated = E.withSeverity E.CRITICAL err
      in E.qiErrorSeverity updated === E.CRITICAL
      
  , QC.testProperty "chain preserves both errors in relationship" $ \err1 err2 ->
      let chained = E.chain err1 err2
      in E.qiErrorCause chained === Just err1
  ]