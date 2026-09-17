-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.FeatureFlags.IsolatedMembers where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testIsolatedMembersInternal :: (HasCallStack) => App ()
testIsolatedMembersInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus alice tid "isolatedMembers" "unlocked"
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "isolatedMembers" enabled
    setFlag InternalAPI ws tid "isolatedMembers" disabled
  Internal.setTeamFeatureLockStatus alice tid "isolatedMembers" "locked"
  setFeature InternalAPI alice tid "isolatedMembers" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "feature-locked"
  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "isolatedMembers" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"

testPatchIsolatedMembers :: (HasCallStack) => App ()
testPatchIsolatedMembers = checkPatch OwnDomain "isolatedMembers" disabled
