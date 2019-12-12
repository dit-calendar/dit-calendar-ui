{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Repository.CalendarRepoSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec
import           Test.HUnit.Base                    (assertEqual, assertFailure)

import           Control.Monad.Identity             (Identity)
import           Control.Monad.Writer.Class         (tell)
import           Data.Default                       (def)

import           AppContext                         (AppContext)
import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.Types                  (ResultError (PermissionAccessInsufficient))
import           Data.Domain.User                   as User
import           Data.Repository.Acid.CalendarEntry (CalendarDAO,
                                                     DeleteEntry (..),
                                                     NewEntry (..),
                                                     UpdateEntry (..))
import           Data.Time.Clock                    (UTCTime)

import qualified Data.Repository.CalendarRepo       as CalendarRepo


oldDate = read "2011-03-20 18:11:42.202854 UTC"::UTCTime
newDate = read "2012-11-19 17:51:42.203841 UTC"::UTCTime

mkFixture "Fixture" [ts| CalendarDAO, AppContext |]

user = def { loginName="Foo", User.userId=10 }

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _create = \(NewEntry caledarEntry) -> return caledarEntry
                  , _delete = \(DeleteEntry a) -> tell [show a]
                  , _update = \(UpdateEntry a)-> tell [show a] >>= (\_ -> return $ Right a)
                  ,_query = undefined
                  , _getCurrentUser = return $ Just user
                }

spec = describe "CalendarRepo" $ do
    it "newCalendarEntry" $ do
        let (result, _) = evalTestFixture (CalendarRepo.createCalendarEntryImpl
                def {startDate=oldDate, endDate=oldDate, description="termin1", CalendarEntry.owner=10}) fixture
        CalendarEntry.description result `shouldBe` "termin1"
        CalendarEntry.owner result `shouldBe` 10
        CalendarEntry.tasks result `shouldBe` []
        CalendarEntry.startDate result `shouldBe` oldDate
    it "deleteCalendarEntry" $ do
        let (_, log) = evalTestFixture (CalendarRepo.deleteCalendarEntryByIdImpl 15) fixture
        log `shouldBe` ["15"::String]
    it "updateCalendar" $ do
        let calc = def { description="termin2", entryId=1, CalendarEntry.owner=10, startDate=oldDate, endDate=oldDate}
        let (result, log) = evalTestFixture (CalendarRepo.updateCalendarImpl calc) fixture
        length log `shouldBe` 1
        assertEqual "update calendar with wrong calendar" (log!!0) (show calc)
        case result of
            Left _ -> assertFailure "updated calendar should be returned"
            Right r -> assertEqual "updated calendar should be returned" r calc
    it "updateCalendarWithoutPermission" $ do
        let calc = def { description="termin2", entryId=1, CalendarEntry.owner=2, startDate=oldDate, endDate=oldDate}
        let (result, log) = evalTestFixture (CalendarRepo.updateCalendarImpl calc) fixture
        length log `shouldBe` 0
        case result of
            Left error -> assertEqual "wrong error returned" error PermissionAccessInsufficient
            Right _ -> assertFailure "updated calendar should be returned"
    it "addTaskToCalendarEntry" $ do
        let calc = def{ description="termin2", entryId=1, CalendarEntry.owner=2, tasks=[1],
            startDate=oldDate, endDate=oldDate}
        let (_, log) = evalTestFixture (CalendarRepo.addTaskToCalendarEntryImpl calc 2) fixture
        let newCalc = calc {tasks = [2, 1]}
        log!!0 `shouldBe` show newCalc
    it "deleteTaskFromCalendarEntry" $ do
        let calc = def{ description="termin2", entryId=1, CalendarEntry.owner=2, tasks=[1,2,3],
            startDate=oldDate, endDate=oldDate}
        let (_, log) = evalTestFixture (CalendarRepo.deleteTaskFromCalendarEntryImpl calc 2) fixture
        let newCalc = calc {tasks = [1, 3]}
        log!!0 `shouldBe` show newCalc
