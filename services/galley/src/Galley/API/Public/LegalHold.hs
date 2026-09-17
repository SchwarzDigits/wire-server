-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.Public.LegalHold where

import Data.Id (TeamId, UserId)
import Data.Qualified (Local, tUnqualified)
import Galley.API.LegalHold
import Galley.App
import Imports
import Polysemy
import Wire.API.Error (ErrorS, throwS)
import Wire.API.Error.Galley
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley.LegalHold
import Wire.API.Team.LegalHold (UserLegalHoldStatusResponse)
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem, isIsolatedNonAdmin)
import Wire.TeamSubsystem (TeamSubsystem, getUserStatus)
import Wire.TeamSubsystem qualified as TeamSubsystem

legalHoldAPI :: API LegalHoldAPI GalleyEffects
legalHoldAPI =
  mkNamedAPI @"create-legal-hold-settings" createSettings
    <@> mkNamedAPI @"get-legal-hold-settings" getSettings
    <@> mkNamedAPI @"delete-legal-hold-settings" removeSettingsInternalPaging
    <@> mkNamedAPI @"get-legal-hold" getUserStatusChecked
    <@> mkNamedAPI @"consent-to-legal-hold" grantConsent
    <@> mkNamedAPI @"request-legal-hold-device" requestDevice
    <@> mkNamedAPI @"disable-legal-hold-for-user" disableForUser
    <@> mkNamedAPI @"approve-legal-hold-device" approveDevice

-- | Like 'getUserStatus', but members of an isolated team cannot use it to
-- find out whether other users are part of their team.
getUserStatusChecked ::
  ( Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (ErrorS 'TeamMemberNotFound) r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Sem r UserLegalHoldStatusResponse
getUserStatusChecked lusr tid uid = do
  when (uid /= tUnqualified lusr) $ do
    mSelf <- TeamSubsystem.internalGetTeamMember (tUnqualified lusr) tid
    isolated <- maybe (pure False) (isIsolatedNonAdmin tid) mSelf
    when isolated $ throwS @'TeamMemberNotFound
  getUserStatus lusr tid uid
