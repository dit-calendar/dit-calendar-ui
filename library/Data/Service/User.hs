{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.User ( deleteUserImpl, UserService(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                   (fromJust)

import           Presentation.AcidHelper      (CtrlV')

import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as MonadDBTaskRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo
import           Data.Service.Task            (TaskService)
import qualified Data.Service.Task            as TaskService


deleteUserImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadDBCalendarRepo m, TaskService m) =>
            User -> m ()
deleteUserImpl user = let calendarToDelete = calendarEntries user in
    do
        foldr ((>>) . MonadDBCalendarRepo.deleteCalendarEntry)
            (return ()) (calendarEntries user)
        removeUserFromTasks user
        MonadDBUserRepo.deleteUser user

removeUserFromTasks ::(MonadDBTaskRepo m, TaskService m) =>
                     User -> m ()
removeUserFromTasks user = foldr (\ taskId ->
    (>>) (do
        task <- MonadDBTaskRepo.getTask taskId
        TaskService.removeUserFromTask task (userId user)))
    (return ()) (belongingTasks user)

class UserService m where
    deleteUser :: User -> m ()

instance (MonadDBUserRepo CtrlV', MonadDBTaskRepo CtrlV', MonadDBCalendarRepo CtrlV', TaskService CtrlV')
            => UserService CtrlV' where
    deleteUser = deleteUserImpl