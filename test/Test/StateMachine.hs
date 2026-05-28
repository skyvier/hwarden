{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.StateMachine (tests) where

import Data.Function ((&))
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.MockEnv

import qualified Hwarden.Agent as Agent
import qualified Hwarden.Bitwarden as Bitwarden

tests :: TestTree
tests =
  testGroup
    "state transition invariants"
    -- These invariants only care about the top-level phase of AgentState:
    -- whether the outer constructor is Locked or Unlocked. They do not
    -- constrain the nested cache and refresh details inside Unlocked.
    [ testProperty "before any successful unlock, the agent remains locked" $
        propertyRemainsLockedUntilSuccessfulUnlock
    , testProperty "a successful unlock always transitions the top-level phase to unlocked" $
        propertySuccessfulUnlockAlwaysTransitionsToUnlocked
    , testProperty "once unlocked, the top-level phase never returns to locked" $
        propertyUnlockedNeverReturnsToLocked
    , testProperty "non-unlock requests never transition locked to unlocked" $
        propertyNonUnlockRequestsDoNotUnlock
    , testProperty "the final top-level phase matches whether the history contains a successful unlock" $
        propertyFinalPhaseMatchesSuccessfulUnlockHistory
    ]

type UnlockOutcome = Either Agent.UnlockError Agent.SessionKey

type SyncOutcome = Either Bitwarden.SyncError ()

type ListItemsOutcome = Either Agent.ListItemsError [Agent.ItemSummary]

type GetPasswordOutcome =
  Either Bitwarden.GetPasswordError Agent.PasswordValue

data Step
  = UnlockStep UnlockOutcome SyncOutcome ListItemsOutcome
  | StatusStep
  | ListItemsStep ListItemsOutcome
  | GetPasswordStep GetPasswordOutcome
  | UnknownStep
  deriving (Eq, Show, Generic)

instance Arbitrary Step where
  arbitrary =
    oneof
      [ UnlockStep <$> arbitrary <*> arbitrary <*> arbitrary,
        pure StatusStep,
        ListItemsStep <$> arbitrary,
        GetPasswordStep <$> arbitrary,
        pure UnknownStep
      ]
  shrink = genericShrink

data NonUnlockStep
  = NonUnlockStatusStep
  | NonUnlockListItemsStep ListItemsOutcome
  | NonUnlockGetPasswordStep GetPasswordOutcome
  | NonUnlockUnknownStep
  deriving (Eq, Show, Generic)

instance Arbitrary NonUnlockStep where
  arbitrary =
    oneof
      [ pure NonUnlockStatusStep
      , NonUnlockListItemsStep <$> arbitrary
      , NonUnlockGetPasswordStep <$> arbitrary
      , pure NonUnlockUnknownStep
      ]
  shrink = genericShrink

stepRequest :: Step -> Agent.Request
stepRequest (UnlockStep _ _ _) =
  Agent.UnlockRequest
    (Agent.Username "me@example.com")
    (Agent.Password "secret")
stepRequest StatusStep = Agent.Status
stepRequest (ListItemsStep _) = Agent.ListItems
stepRequest (GetPasswordStep _) =
  Agent.GetPasswordRequest (Agent.LoginItemId "item-123")
stepRequest UnknownStep = Agent.UnknownRequest

stepMockEnv :: Step -> MockEnv
stepMockEnv step =
  case step of
    UnlockStep unlockOutcome syncOutcome listItemsOutcome ->
      defaultMockEnv
        & withUnlockResult unlockOutcome
        & withSyncResult syncOutcome
        & withListItemsResult listItemsOutcome
    StatusStep ->
      defaultMockEnv
    ListItemsStep listItemsOutcome ->
      defaultMockEnv
        & withListItemsResult listItemsOutcome
    GetPasswordStep getPasswordOutcome ->
      defaultMockEnv
        & withGetPasswordResult getPasswordOutcome
    UnknownStep ->
      defaultMockEnv

runHistory :: [Step] -> [Agent.AgentState]
runHistory = reverse . snd . foldl runOne (Agent.Locked, [])
  where
    runOne (state0, states) step =
      let (state1, _, _) =
            runMockBitwarden
              (stepMockEnv step)
              (Agent.handleRequestWith (stepRequest step) state0)
       in (state1, state1 : states)

isUnlockedState :: Agent.AgentState -> Bool
isUnlockedState Agent.Locked = False
isUnlockedState Agent.Unlocked {} = True

hasSuccessfulUnlockStep :: Step -> Bool
hasSuccessfulUnlockStep (UnlockStep (Right _) _ _) = True
hasSuccessfulUnlockStep _ = False

nonUnlockStepToStep :: NonUnlockStep -> Step
nonUnlockStepToStep NonUnlockStatusStep = StatusStep
nonUnlockStepToStep (NonUnlockListItemsStep listItemsOutcome) =
  ListItemsStep listItemsOutcome
nonUnlockStepToStep (NonUnlockGetPasswordStep getPasswordOutcome) =
  GetPasswordStep getPasswordOutcome
nonUnlockStepToStep NonUnlockUnknownStep = UnknownStep

allStatesLocked :: [Agent.AgentState] -> Bool
allStatesLocked = all isLockedState

isLockedState :: Agent.AgentState -> Bool
isLockedState Agent.Locked = True
isLockedState Agent.Unlocked {} = False

propertyRemainsLockedUntilSuccessfulUnlock :: [Step] -> Property
propertyRemainsLockedUntilSuccessfulUnlock steps =
  let prefixWithoutSuccess = takeWhile (not . hasSuccessfulUnlockStep) steps
      trace = runHistory prefixWithoutSuccess
   in counterexample
        (unlines ["steps: " <> show prefixWithoutSuccess, "trace: " <> show trace])
        (property (allStatesLocked trace))

propertySuccessfulUnlockAlwaysTransitionsToUnlocked :: [Step] -> Property
propertySuccessfulUnlockAlwaysTransitionsToUnlocked steps =
  let trace = runHistory steps
      transitions = zip steps trace
      successfulUnlockTransitions =
        [ (step, state)
        | (step, state) <- transitions
        , hasSuccessfulUnlockStep step
        ]
   in counterexample
        ( unlines
            [ "steps: " <> show steps
            , "trace: " <> show trace
            , "successful unlock transitions: " <> show successfulUnlockTransitions
            ]
        )
        ( property $
            all (isUnlockedState . snd) successfulUnlockTransitions
        )

propertyUnlockedNeverReturnsToLocked :: [Step] -> Property
propertyUnlockedNeverReturnsToLocked steps =
  let trace = runHistory steps
      suffixes = dropWhile (not . isUnlockedState) trace
   in counterexample
        (unlines ["steps: " <> show steps, "trace: " <> show trace])
        ( property $
            case suffixes of
              [] -> True
              xs -> all isUnlockedState xs
        )

propertyNonUnlockRequestsDoNotUnlock :: [NonUnlockStep] -> Property
propertyNonUnlockRequestsDoNotUnlock steps =
  let history = map nonUnlockStepToStep steps
      trace = runHistory history
   in counterexample
        (unlines ["steps: " <> show history, "trace: " <> show trace])
        (property (allStatesLocked trace))

propertyFinalPhaseMatchesSuccessfulUnlockHistory :: [Step] -> Property
propertyFinalPhaseMatchesSuccessfulUnlockHistory steps =
  let trace = runHistory steps
      sawSuccessfulUnlock = any hasSuccessfulUnlockStep steps
   in counterexample
        (unlines ["steps: " <> show steps, "trace: " <> show trace])
        ( property $
            case reverse trace of
              [] -> not sawSuccessfulUnlock
              finalState : _ ->
                isUnlockedState finalState == sawSuccessfulUnlock
        )
