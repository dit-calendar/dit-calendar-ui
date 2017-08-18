{-# LANGUAGE FlexibleContexts #-}
module Data.Repository.UserRepoHelper ( deleteUser ) where

import Happstack.Foundation     ( HasAcidState )
import Control.Monad.IO.Class

import Data.Domain.User                     as User

import qualified Data.Repository.CalendarRepo         as CalendarRepo
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.Acid.UserAcid        as UserAcid
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid


deleteUser :: (MonadIO m, HasAcidState m CalendarAcid.EntryList, HasAcidState m UserAcid.UserList) =>
     User -> m ()
deleteUser user =
     do
        CalendarRepo.deleteCalendar (calendarEntries user)
        UserRepo.deleteUser user