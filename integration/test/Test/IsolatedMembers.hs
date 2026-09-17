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

-- | Tests for the @isolatedMembers@ team feature: members of an isolated team
-- are not implicitly connected and cannot discover each other.
module Test.IsolatedMembers where

import API.Brig
import qualified API.BrigInternal as BrigI
import API.Galley
import qualified API.GalleyInternal as GalleyI
import MLS.Util
import SetupHelpers
import Testlib.Prelude

-- | A team with an owner and two regular members, with the isolated members
-- feature enabled.
createIsolatedTeam :: (HasCallStack) => App (Value, String, Value, Value)
createIsolatedTeam = do
  (owner, tid, [alice, bob]) <- createTeam OwnDomain 3
  enableIsolation owner tid
  pure (owner, tid, alice, bob)

enableIsolation :: (HasCallStack, MakesValue domain) => domain -> String -> App ()
enableIsolation domain tid = do
  GalleyI.setTeamFeatureLockStatus domain tid "isolatedMembers" "unlocked"
  void $ GalleyI.setTeamFeatureStatus domain tid "isolatedMembers" "enabled" >>= getBody 200

testIsolatedMembersNoMLSOne2One :: (HasCallStack) => App ()
testIsolatedMembersNoMLSOne2One = do
  (owner, _, alice, bob) <- createIsolatedTeam
  getMLSOne2OneConversation alice bob >>= assertLabel 403 "not-connected"
  getMLSOne2OneConversation bob alice >>= assertLabel 403 "not-connected"
  -- admins are not exempt from the isolation
  getMLSOne2OneConversation owner alice >>= assertLabel 403 "not-connected"

-- | The 1-1 conversation is only persisted on the first commit, so fetching it
-- before the team gets isolated must not allow creating it afterwards.
testIsolatedMembersNoMLSOne2OneViaFirstCommit :: (HasCallStack) => App ()
testIsolatedMembersNoMLSOne2OneViaFirstCommit = do
  (owner, tid, [alice, bob]) <- createTeam OwnDomain 3
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  enableIsolation owner tid

  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  resetOne2OneGroup def alice1 one2OneConv
  commit <- createAddCommit alice1 one2OneConvId [bob]
  postMLSCommitBundle commit.sender (mkBundle commit) >>= assertLabel 403 "access-denied"

-- | Independent of the isolation feature: the first commit of a 1-1
-- conversation must be rejected if the users are not connected (anymore).
testMLSOne2OneFirstCommitRequiresConnection :: (HasCallStack) => App ()
testMLSOne2OneFirstCommitRequiresConnection = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  one2OneConv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  void $ putConnection alice bob "blocked" >>= getBody 200

  one2OneConvId <- objConvId $ one2OneConv %. "conversation"
  resetOne2OneGroup def alice1 one2OneConv
  commit <- createAddCommit alice1 one2OneConvId [bob]
  postMLSCommitBundle commit.sender (mkBundle commit) >>= assertLabel 403 "access-denied"

testIsolatedMembersCannotAddEachOther :: (HasCallStack) => App ()
testIsolatedMembersCannotAddEachOther = do
  (_, tid, alice, bob) <- createIsolatedTeam

  -- MLS: adding a team member via a commit
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  convId <- createNewGroupWith def alice1 defMLS {team = Just tid}
  commit <- createAddCommit alice1 convId [bob]
  postMLSCommitBundle commit.sender (mkBundle commit) >>= assertLabel 403 "not-connected"

  -- Proteus: creating a conversation with a team member, and adding one later
  postConversation alice defProteus {team = Just tid, qualifiedUsers = [bob]}
    >>= assertLabel 403 "not-connected"
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  addMembers alice conv def {users = [bob]} >>= assertLabel 403 "not-connected"

testIsolatedMembersTeamMemberVisibility :: (HasCallStack) => App ()
testIsolatedMembersTeamMemberVisibility = do
  (owner, tid, alice, bob) <- createIsolatedTeam
  [ownerId, aliceId, bobId] <- traverse objId [owner, alice, bob]

  -- a regular member only sees themselves
  bindResponse (getTeamMembers alice tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members" >>= asList
    uids <- for members (%. "user")
    uids `shouldMatchSet` [aliceId]
  getTeamMember alice tid bobId >>= assertLabel 404 "no-team-member"
  void $ getTeamMember alice tid aliceId >>= getJSON 200

  -- the legal hold status does not reveal team membership either
  legalholdUserStatus tid alice bob >>= assertLabel 404 "no-team-member"
  void $ legalholdUserStatus tid alice alice >>= getJSON 200

  -- the owner still sees everybody
  void $ legalholdUserStatus tid owner bob >>= getJSON 200
  bindResponse (getTeamMembers owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members" >>= asList
    uids <- for members (%. "user")
    uids `shouldMatchSet` [ownerId, aliceId, bobId]
  void $ getTeamMember owner tid bobId >>= getJSON 200

testIsolatedMembersSearch :: (HasCallStack) => App ()
testIsolatedMembersSearch = do
  (owner, _, alice, bob) <- createIsolatedTeam
  BrigI.refreshIndex OwnDomain
  bobId <- objId bob

  bindResponse (searchContacts alice (bob %. "name") OwnDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    docs <- resp.json %. "documents" >>= asList
    docs `shouldMatch` ([] :: [Value])

  bindResponse (searchContacts owner (bob %. "name") OwnDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    docs <- resp.json %. "documents" >>= asList
    uids <- for docs objId
    uids `shouldMatchSet` [bobId]

testIsolatedMembersTeamConversations :: (HasCallStack) => App ()
testIsolatedMembersTeamConversations = do
  (owner, tid, alice, _) <- createIsolatedTeam
  let getTeamConvs user = do
        req <- baseRequest user Galley Versioned (joinHttpPath ["teams", tid, "conversations"])
        submit "GET" req
  getTeamConvs alice >>= assertLabel 403 "operation-denied"
  void $ getTeamConvs owner >>= getJSON 200

testIsolatedMembersTeamNotifications :: (HasCallStack) => App ()
testIsolatedMembersTeamNotifications = do
  (owner, _, alice, _) <- createIsolatedTeam
  bindResponse (getTeamNotifications alice Nothing) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "notifications" `shouldMatch` ([] :: [Value])
  -- the owner still sees the member-join events
  bindResponse (getTeamNotifications owner Nothing) $ \resp -> do
    resp.status `shouldMatchInt` 200
    notifs <- resp.json %. "notifications" >>= asList
    assertBool "the owner should see the team events" (not (null notifs))
