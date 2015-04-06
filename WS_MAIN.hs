{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Yesod
import Database.Persist.Sqlite
import Data.Text                    (Text, unpack, pack, splitOn, length)
import Control.Applicative          ((<$>), (<*>))
import System.IO                    (writeFile, appendFile)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger         (runStderrLoggingT)
import System.Time                  (getClockTime)

import WS_CSS



{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - database ------------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    userId Text
    password Text
    role Text
	UniqueUserId userId
Workspace
    name Text
    note Text
    status Text
    assignee Text
    UniqueName name
Assignment
    userId UserId
    workspaceId WorkspaceId
    right Text
    wsOrder Text
    UniqueUserWorkspace userId workspaceId
|]


queryWorkspaceAssignmentByUserStmt :: Text
queryWorkspaceAssignmentByUserStmt = "\
    \ SELECT ??, ?? \
    \ FROM Workspace, Assignment \
    \ ON Workspace.id = Assignment.workspace_id \
    \ WHERE Assignment.user_id = ? \
    \ ORDER BY Assignment.ws_order \
    \"

queryWorkspaceAssignmentByUserOuterStmt :: Text
queryWorkspaceAssignmentByUserOuterStmt = "\
    \ SELECT ??, a.right \
    \ FROM Workspace LEFT OUTER JOIN (SELECT Assignment.workspace_id, Assignment.right FROM Assignment WHERE Assignment.user_id = ?) a \
    \ ON Workspace.id = a.workspace_id \
    \ "

{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - yesod ---------------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}

data WS = WS {
    pool :: ConnectionPool
}

mkYesod "WS" [parseRoutes|
/                           HomeR           GET
/login                      LoginR          GET POST
/logout                     LogoutR         GET
/newuser                    NewUserR        GET POST
/user/#Text                 UserR           GET POST
/user/#Text/delete          DeleteUserR     GET
/user/#Text/assign/#Text    AssignUserR     POST
/user/#Text/pw              ResetPasswordR  GET
/myuser                     MyUserR         GET POST
/newws                      NewWsR          GET POST
/ws/#Text                   WsR             GET POST
/ws/rest/#Text              WsRestR         GET POST
/ws/order/#Text             WsOrderR        POST
|]

instance Yesod WS where
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend minutes filepath
        where minutes    = 24 * 60      -- 1 day
              filepath   = "client_session_key.aes"

instance YesodPersist WS where
    type YesodPersistBackend WS = SqlBackend
    runDB action = do
        yesod <- getYesod
        runSqlPool action (pool yesod)

instance RenderMessage WS FormMessage where
    renderMessage _ _ = defaultFormMessage


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - data / type definition ----------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


data Response = Response
    { statusCode    :: Text
    , statusMsg     :: Text
    , workspace     :: Workspace
    } deriving (Generic)
    
instance ToJSON Response

instance ToJSON Workspace where
    toJSON (Workspace name note status assignee) = object 
        [ "name"    .= name
        , "note"    .= note
        , "status"  .= status
        , "assignee".= assignee
        ]


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - resource handler ----------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


------------------------------------------------------------------------------
-- home ----------------------------------------------------------------------
------------------------------------------------------------------------------
 

getHomeR :: Handler Html
getHomeR = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = userRole $ entityVal u
                    wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                    wsAllList <- runDB $ selectList [] [Asc WorkspaceName] 
                    userList <- runDB $ selectList [] [Asc UserUserId] 
                    defaultLayout $ do
                        defaultHeader sessionUserId
                        tabRow "" wsList (unpack role)
                        [whamlet|
                            $if role == "Admin"
                                <div>
                                    <ul ."list">Workspaces
                                        $forall Entity wsKey wsVal <- wsAllList
                                            <li>
                                                <a href="/ws/#{workspaceName wsVal}">#{workspaceName wsVal}
                                <div>
                                    <ul ."list">Users
                                        $forall Entity userKey userVal <- userList
                                            <li>
                                                <a href="/user/#{userUserId userVal}">#{userUserId userVal}
                        |]
                        defaultFooter


------------------------------------------------------------------------------
-- login ---------------------------------------------------------------------
------------------------------------------------------------------------------


getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout $ do
        defaultHeader ""
        [whamlet|
            <table>
                <tbody>
                    <tr>
                        <td>
                            <form method=post action=@{LoginR} enctype=#{enctype}>
                                ^{widget}
                                <button>Submit
        |]
        defaultFooter


postLoginR :: Handler Html
postLoginR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of
        FormSuccess login -> do
            user <- runDB $ selectFirst [UserUserId ==. userUserId login, UserPassword ==. userPassword login] []
            case user of
                Nothing -> do
                    setMessage "Invalid userid/password."
                    redirect LoginR
                _ -> do
                    setSession "userId" $ userUserId login
                    redirectUltDest HomeR
        _ -> do
            setMessage "Invalid input. (WS-002)"
            redirect LoginR


------------------------------------------------------------------------------
-- logout --------------------------------------------------------------------
------------------------------------------------------------------------------


getLogoutR :: Handler Html
getLogoutR = do
    clearUltDest
    setMessage "Logged out"
    deleteSession "userId"
    redirect LoginR
    

------------------------------------------------------------------------------
-- new user ------------------------------------------------------------------
------------------------------------------------------------------------------


getNewUserR :: Handler Html
getNewUserR = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = unpack $ userRole $ entityVal u
                    if role /= "Admin"
                    then do
                        setMessage "You are not allowed to create new user."
                        redirect HomeR
                    else do
                        wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                        defaultLayout $ do
                            defaultHeader sessionUserId
                            tabRow "" wsList role
                            userNewForm
                            defaultFooter


postNewUserR :: Handler Html
postNewUserR = do
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> redirect LoginR
        Just sessionUserId  -> do
            userId <- runInputPost $ ireq textField "userId"
            role <- runInputPost $ ireq textField "role"
            isSave <- runInputPost $ iopt textField "save"
            isSaveAndNew <- runInputPost $ iopt textField "saveAndNew"
            if isJust2 isSave isSaveAndNew
            then do
                newUser <- runDB $ insertUnique $ User userId userId role
                case newUser of
                    Nothing     -> do
                        setMessage "UserId already in use."
                        redirect NewUserR
                    Just newU   -> do
                        setMessage "User created"
                        if isSave /= Nothing
                        then redirect $ UserR userId
                        else redirect NewUserR
            else do
                setMessage "Unexpected error. (WS-001)"
                redirect LoginR
    

------------------------------------------------------------------------------
-- user ----------------------------------------------------------------------
------------------------------------------------------------------------------


getUserR :: Text -> Handler Html
getUserR requestedUserId = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = unpack $ userRole $ entityVal u
                    if role /= "Admin"
                    then do
                        setMessage "You don't have the privilege to see this user profile."
                        redirect HomeR
                    else do
                        requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                        case requestedUser of
                            Nothing         -> do
                                setMessage "The requested user doesn't exist."
                                redirect HomeR
                            Just requestedU -> do
                                wsAssList <- queryWorkspaceAssignmentByUserOuter requestedU
                                wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                                defaultLayout $ do
                                    defaultHeader sessionUserId
                                    tabRow "" wsList role
                                    [whamlet| <div ."subTitleClass" id="userHeader">User: #{unpack requestedUserId} |]
                                    userEditForm (unpack requestedUserId) (unpack $ userRole $ entityVal requestedU)
                                    [whamlet| <div ."subTitleClass" id="assignmentHeader">Assignments |]
                                    [whamlet|
                                        <table id="assignmentTable">
                                            <tbody>
                                                $forall (Entity _ wsVal, right) <- wsAssList
                                                    <tr>
                                                        <td>#{workspaceName wsVal}
                                                        <td>
                                                            <select id="right" name="right">
                                                                <option name="Nothing" value="Nothing">
                                                                $if right == Just (Single (pack "ReadOnly"))
                                                                    <option name="ReadOnly" value="ReadOnly" selected="selected">ReadOnly
                                                                $else
                                                                    <option name="ReadOnly" value="ReadOnly">ReadOnly
                                                                $if right == Just (Single (pack "Edit"))
                                                                    <option name="Edit" value="Edit" selected="selected">Edit
                                                                $else
                                                                    <option name="Edit" value="Edit">Edit
                                                                $if right == Just (Single (pack "Admin"))
                                                                    <option name="Admin" value="Admin" selected="selected">Admin
                                                                $else
                                                                    <option name="Admin" value="Admin">Admin
                                    |]
                                    [whamlet| ^{assignmentJS} |]
                                    defaultFooter


postUserR :: Text -> Handler Html
postUserR requestedUserId = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                    case requestedUser of
                        Nothing         -> do
                            setMessage "Unexpected error. (WS-004)"
                            redirect $ UserR requestedUserId
                        Just requestedU -> do
                            role <- runInputPost $ ireq textField "role"
                            isSave <- runInputPost $ iopt textField "saveUser"
                            runDB $ updateWhere [UserUserId ==. requestedUserId] [UserRole =. role]
                            redirect $ UserR requestedUserId


------------------------------------------------------------------------------
-- delete user ---------------------------------------------------------------
------------------------------------------------------------------------------


getDeleteUserR :: Text -> Handler Html
getDeleteUserR requestedUserId = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = unpack $ userRole $ entityVal u
                    if role /= "Admin"
                    then do
                        setMessage "You don't have the privilege to delete this user profile."
                        redirect HomeR
                    else do
                        requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                        case requestedUser of
                            Nothing         -> do
                                setMessage "Unexpected error. (WS-006)"
                                redirect $ UserR requestedUserId
                            Just requestedU -> do
                                runDB $ deleteWhere [AssignmentUserId ==. entityKey requestedU]
                                runDB $ delete $ entityKey requestedU
                                redirect HomeR


------------------------------------------------------------------------------
-- assign user ---------------------------------------------------------------
------------------------------------------------------------------------------


postAssignUserR :: Text -> Text -> Handler RepPlain
postAssignUserR requestedUserId input = do
    let workspace   = head (splitOn "&" input)
    let right       = last (splitOn "&" input)
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> return $ RepPlain "Not logged in. (WS-000)"
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    return $ RepPlain "Unexpected error. (WS-001)"
                Just u  -> do
                    requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                    case requestedUser of
                        Nothing         -> do
                            return $ RepPlain "Unexpected error. (WS-004)"
                        Just requestedU -> do
                            wsM <- runDB $ getBy $ UniqueName workspace
                            case wsM of
                                Nothing -> do
                                    return $ RepPlain "Workspace doesn't exist."
                                Just ws -> do                                    
                                    if (unpack right) == "Nothing"
                                    then do
                                        deleteAss <- runDB $ selectFirst [AssignmentUserId ==. entityKey requestedU, AssignmentWorkspaceId ==. entityKey ws] []
                                        case deleteAss of
                                            Nothing         -> do
                                                return $ RepPlain "Unexpected error. (WS-005)"
                                            Just deleteA    -> do
                                                runDB $ delete (entityKey deleteA)
                                                return $ RepPlain "OK"
                                    else do
                                        newAss <- runDB $ insertUnique $ Assignment (entityKey requestedU) (entityKey ws) right "0"
                                        case newAss of
                                            Just newA   -> return $ RepPlain "OK"
                                            Nothing     -> do
                                                runDB $ updateWhere [AssignmentWorkspaceId ==. entityKey ws] [AssignmentRight =. right]
                                                return $ RepPlain "OK"
                                                    

------------------------------------------------------------------------------
-- my user -------------------------------------------------------------------
------------------------------------------------------------------------------


getMyUserR :: Handler Html
getMyUserR = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = userRole $ entityVal u
                    wsAssList <- queryWorkspaceAssignmentByUser u
                    defaultLayout $ do
                        defaultHeader sessionUserId
                        tabRow "" (map fst wsAssList) (unpack role)
                        [whamlet| <div ."subTitleClass" id="userHeader">User: #{unpack $ userUserId $ entityVal u} |]
                        myUserForm (unpack $ userUserId $ entityVal u) (unpack $ userRole $ entityVal u)
                        [whamlet|
                            <div ."subTitleClass" id="assignmentHeader">Assignments
                            <table id="assignmentTable">
                                <tbody>
                                    $forall (Entity _ wsVal, Entity _ assVal) <- wsAssList
                                        <tr>
                                            <td>#{workspaceName wsVal}
                                            <td>#{assignmentRight assVal}
                        |]
                        [whamlet| ^{workspaceOrderJS} |]
                        [whamlet|
                            <div ."subTitleClass" id="workspaceHeader">Workspace Order
                            <table id="workspaceOrderTable">
                                <tbody>
                                    <tr>
                                        <td>
                                            <select id="workspaceOrder" multiple="yes" size="5">
                                                $forall (Entity _ wsVal, Entity _ _) <- wsAssList
                                                    <option value="#{workspaceName wsVal}">#{workspaceName wsVal}
                                        <td>
                                            <input type="button" ."btnShort" id="moveUp" name="moveUp" value="Up" onclick="moveUp();">
                                            <input type="button" ."btnShort" id="moveDown" name="moveDown" value="Down" onclick="moveDown();">
                                            <input type="button" ."btnShort" id="saveWSOrder" name="save" value="Save" onclick="saveWSOrder();">
                        |]
                        defaultFooter


postMyUserR :: Handler Html
postMyUserR = do
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    password <- runInputPost $ ireq textField "newPassword"
                    passwordR <- runInputPost $ ireq textField "newPasswordR"
                    if password /= passwordR
                    then do
                        setMessage "New password and repeated new password are not equal."
                        redirect MyUserR
                    else do    
                        runDB $ updateWhere [UserUserId ==. (userUserId $ entityVal u)] [UserPassword =. password]
                        setMessage "Password updated"
                        redirect MyUserR
                            

------------------------------------------------------------------------------
-- reset password ------------------------------------------------------------
------------------------------------------------------------------------------


getResetPasswordR :: Text -> Handler Html
getResetPasswordR requestedUserId = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = unpack $ userRole $ entityVal u
                    if role /= "Admin"
                    then do
                        setMessage "You don't have the privilege to reset the password for an user."
                        redirect HomeR
                    else do
                        runDB $ updateWhere [UserUserId ==. requestedUserId] [UserPassword =. requestedUserId]
                        setMessage "Password reseted"
                        redirect $ UserR requestedUserId
                        

------------------------------------------------------------------------------
-- new ws --------------------------------------------------------------------
------------------------------------------------------------------------------


getNewWsR :: Handler Html
getNewWsR = do
    clearUltDest
    (widget, enctype) <- generateFormPost workspaceForm
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = userRole $ entityVal u
                    wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                    defaultLayout $ do
                        defaultHeader sessionUserId
                        tabRow "" wsList (unpack role)
                        [whamlet|
                            <table>
                                <tbody>
                                    <tr>
                                        <td>
                                            <form method=post action=@{NewWsR} enctype=#{enctype}>
                                                ^{widget}
                                                <button>Submit
                        |]
                        [whamlet| ^{newWsJS} |]
                        defaultFooter


postNewWsR :: Handler Html
postNewWsR = do
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    ((result, widget), enctype) <- runFormPost workspaceForm
                    case result of
                        FormSuccess ws -> do
                            wsId <- runDB $ insertUnique ws
                            case wsId of
                                Nothing -> do
                                    setMessage "This name is already in use."
                                    redirect NewWsR
                                Just workspace -> do
                                    runDB $ insert $ Assignment (entityKey u) workspace "Admin" "0"
                                    redirect $ WsR (workspaceName ws)
                        FormFailure msg -> defaultLayout $ do
                            setMessage $ toHtml $ head msg
                            redirect NewWsR
                        _ -> defaultLayout $ do
                            setMessage "Invalid input. (WS-002)"
                            redirect NewWsR


------------------------------------------------------------------------------
-- ws ------------------------------------------------------------------------
------------------------------------------------------------------------------


getWsR :: Text -> Handler Html
getWsR requestedWsName = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = userRole $ entityVal u
                    requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                    case requestedWorkspace of
                        Nothing         -> do
                            setMessage "The requested workspace doesn't exist."
                            redirect HomeR
                        Just requestedWs -> do
                            wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                            assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                            case assignment of
                                Nothing     -> do
                                    setMessage "You don't have the privilege to see this workspace."
                                    redirect HomeR
                                Just a      -> do
                                    let note = workspaceNote $ entityVal requestedWs
                                    let right = assignmentRight $ entityVal a
                                    defaultLayout $ do
                                        defaultHeader sessionUserId
                                        tabRow (unpack requestedWsName) wsList (unpack role)
                                        workspaceWorkForm (entityVal requestedWs) (unpack right)
                                        defaultFooter
                                        
                                        
postWsR :: Text -> Handler Html
postWsR requestedWorkspaceName = do
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-007)"
                    redirect $ WsR requestedWorkspaceName
                Just u  -> do
                    requestedWorkspace <- runDB $ getBy $ UniqueName requestedWorkspaceName
                    case requestedWorkspace of
                        Nothing             -> redirect HomeR
                        Just requestedWs    -> do
                            let status = workspaceStatus $ entityVal requestedWs
                            let note = workspaceNote $ entityVal requestedWs
                            let assignee = workspaceAssignee $ entityVal requestedWs
                            newNote <- fmap getText (runInputPost $ iopt textareaField "noteArea")
                            isGet <- runInputPost $ iopt textField "get"
                            isCheckIn <- runInputPost $ iopt textField "checkIn"
                            isCheckOut <- runInputPost $ iopt textField "checkOut"
                            isDiscardCheckOut <- runInputPost $ iopt textField "discardCheckOut"
                            isDelete <- runInputPost $ iopt textField "delete"
                            wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                            assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                            case assignment of
                                Nothing -> do
                                    setMessage "Unexpected error. (WS-008)"
                                    redirect $ WsR requestedWorkspaceName
                                Just a  -> do
                                    let right = assignmentRight $ entityVal a
                                    case isGet of
                                        Just g -> do
                                            setMessage "Get done"
                                            redirect $ WsR requestedWorkspaceName
                                        _ -> case isCheckIn of
                                            Just i -> do
                                                case unpack status of
                                                    "0"       -> do
                                                        setMessage "Please check out first"
                                                        redirect $ WsR requestedWorkspaceName
                                                    "1"  -> do
                                                        if assignee == sessionUserId
                                                        then do
                                                            liftIO $ writeWSFile (unpack requestedWorkspaceName) newNote (unpack assignee)
                                                            runDB $ updateWhere [WorkspaceName ==. requestedWorkspaceName] [WorkspaceStatus =. pack "0", WorkspaceAssignee =. pack "null", WorkspaceNote =. pack newNote]
                                                            setMessage "Checked in"
                                                            redirect $ WsR requestedWorkspaceName
                                                        else do
                                                            setMessage $ toHtml ("The file is already checked out by " ++ (unpack assignee))
                                                            redirect $ WsR requestedWorkspaceName
                                            _ -> case isCheckOut of
                                                Just o -> do
                                                     case unpack status of
                                                        "0" -> do
                                                            if right == pack "ReadOnly"
                                                                then do
                                                                    setMessage "You do not have the right to check out the note."
                                                                    redirect $ WsR requestedWorkspaceName
                                                                else do
                                                                    runDB $ updateWhere [WorkspaceName ==. requestedWorkspaceName] [WorkspaceStatus =. pack "1", WorkspaceAssignee =. sessionUserId]
                                                                    setMessage "Checked out"
                                                                    redirect $ WsR requestedWorkspaceName   
                                                        _  -> do
                                                            setMessage $ toHtml ("The file is already checked out by " ++ (unpack assignee))
                                                            redirect $ WsR requestedWorkspaceName
                                                _ -> case isDiscardCheckOut of
                                                    Just c -> do
                                                        runDB $ updateWhere [WorkspaceName ==. requestedWorkspaceName] [WorkspaceStatus =. pack "0", WorkspaceAssignee =. "null"]
                                                        setMessage "Check out discarded"
                                                        redirect $ WsR requestedWorkspaceName
                                                    _ -> case isDelete of
                                                        Just d -> do
                                                            runDB $ deleteWhere [AssignmentWorkspaceId ==. entityKey requestedWs]
                                                            runDB $ deleteWhere [WorkspaceName ==. requestedWorkspaceName]
                                                            redirect HomeR
                                                        _ -> do
                                                            setMessage "Unexpected error. (WS-009)"
                                                            redirect $ WsR requestedWorkspaceName


getWsRestR :: Text -> Handler TypedContent
getWsRestR input = do
    let requestedWsName = (splitOn "&" input)!!0
    let action          = (splitOn "&" input)!!1
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> selectRep $ provideRep $ returnJson $ toJSON $ Response "001" "Not logged in" (Workspace "" "" "" "")
        Just sessionUserId  -> case action of
            "get" -> do
                uid <- runDB $ getBy $ UniqueUserId sessionUserId
                case uid of
                    Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ Response "100" "Unexpected error." (Workspace "" "" "" "")
                    Just u  -> do
                        let role = userRole $ entityVal u
                        requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                        case requestedWorkspace of
                            Nothing         -> selectRep $ provideRep $ returnJson $ toJSON $ Response "123" (pack ("The requested workspace '" ++ (unpack requestedWsName) ++ "' doesn' exist")) (Workspace "" "" "" "")
                            Just requestedWs -> do
                                wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                                assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                                case assignment of
                                    Nothing     -> selectRep $ provideRep $ returnJson $ toJSON $ Response "122" "You do not have the privilege to see the note." (Workspace "" "" "" "")
                                    Just a      -> do
                                        selectRep $ provideRep $ returnJson $ toJSON $ Response "000" "Get" (entityVal requestedWs)
            "checkOut" -> do
                uid <- runDB $ getBy $ UniqueUserId sessionUserId
                case uid of
                    Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ Response "100" "Unexpected error." (Workspace "" "" "" "")
                    Just u  -> do
                        requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                        case requestedWorkspace of
                            Nothing             -> redirect HomeR
                            Just requestedWs    -> do
                                let note = workspaceNote $ entityVal requestedWs
                                let status = workspaceStatus $ entityVal requestedWs
                                let assignee = workspaceAssignee $ entityVal requestedWs
                                wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                                assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                                case assignment of
                                    Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ Response "122" "You do not have the privilege to see the note." (Workspace "" "" "" "")
                                    Just a  -> do
                                        let right = assignmentRight $ entityVal a
                                        case unpack status of
                                            "0" -> do
                                                if right == pack "ReadOnly"
                                                    then selectRep $ provideRep $ returnJson $ toJSON $ Response "121" "You do not have the privilege to check out the note." (entityVal requestedWs)
                                                    else do
                                                        runDB $ updateWhere [WorkspaceName ==. requestedWsName] [WorkspaceStatus =. pack "1", WorkspaceAssignee =. sessionUserId]
                                                        selectRep $ provideRep $ returnJson $ toJSON $ Response "000" "Checked out" (entityVal requestedWs)
                                            _  -> selectRep $ provideRep $ returnJson $ toJSON $ Response "120" (pack ("The file is already checked out by " ++ (unpack assignee))) (entityVal requestedWs)
            _ -> selectRep $ provideRep $ returnJson $ toJSON $ Response "200" (pack ("Action '" ++ (unpack action) ++ "' not supported")) (Workspace "" "" "" "")
                                        
                                        

postWsRestR :: Text -> Handler TypedContent
postWsRestR input = do
    let requestedWsName = (splitOn "&" input)!!0
    let action          = (splitOn "&" input)!!1
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> selectRep $ provideRep $ returnJson $ toJSON $ Response "001" "Not logged in" (Workspace "" "" "" "")
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ Response "100" "Unexpected error." (Workspace "" "" "" "")
                Just u  -> do
                    requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                    case requestedWorkspace of
                        Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ Response "123" (pack ("The requested workspace '" ++ (unpack requestedWsName) ++ "' doesn' exist")) (Workspace "" "" "" "")
                        Just requestedWs    -> do
                            let status = workspaceStatus $ entityVal requestedWs
                            let note = workspaceNote $ entityVal requestedWs
                            let assignee = workspaceAssignee $ entityVal requestedWs
                            let newNote = (splitOn "&" input)!!2
                            wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                            assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                            case assignment of
                                Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ Response "122" "You do not have the privilege to see the note." (Workspace "" "" "" "")
                                Just a  -> do
                                    let right = assignmentRight $ entityVal a
                                    do
                                        case unpack status of
                                            "0"       -> selectRep $ provideRep $ returnJson $ toJSON $ Response "124" "Please check out first" (entityVal requestedWs)
                                            "1"  -> do
                                                if assignee == sessionUserId
                                                then do
                                                    case action of
                                                        "checkIn" -> do
                                                            liftIO $ writeWSFile (unpack requestedWsName) (unpack newNote) (unpack assignee)
                                                            runDB $ updateWhere [WorkspaceName ==. requestedWsName] [WorkspaceStatus =. pack "0", WorkspaceAssignee =. pack "null", WorkspaceNote =. newNote]
                                                            selectRep $ provideRep $ returnJson $ toJSON $ Response "000" "Checked in" (entityVal requestedWs)
                                                        "save" -> do
                                                            liftIO $ writeWSFile (unpack requestedWsName) (unpack newNote) (unpack assignee)
                                                            runDB $ updateWhere [WorkspaceName ==. requestedWsName] [WorkspaceNote =. newNote]
                                                            selectRep $ provideRep $ returnJson $ toJSON $ Response "000" "Checked in & Checked out" (entityVal requestedWs)
                                                        _ -> selectRep $ provideRep $ returnJson $ toJSON $ Response "200" (pack ("Action '" ++ (unpack action) ++ "' not supported")) (Workspace "" "" "" "")
                                                else selectRep $ provideRep $ returnJson $ toJSON $ Response "000" (pack ("The file is already checked out by " ++ (unpack assignee))) (entityVal requestedWs)


------------------------------------------------------------------------------
-- ws order ------------------------------------------------------------------
------------------------------------------------------------------------------


postWsOrderR :: Text -> Handler RepPlain
postWsOrderR input = do
    let inputA = (splitOn "&" input)
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> return $ RepPlain "Not logged in. (WS-000)"
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    return $ RepPlain "Unexpected error. (WS-001)"
                Just u  -> do
                    updateWsOrder inputA u


updateWsOrder :: [Text] -> (Entity User) -> Handler RepPlain
updateWsOrder (wsName:orderWs:inputA) u = do
    workspace <- runDB $ getBy $ UniqueName wsName
    case workspace of
        Nothing -> return $ RepPlain "Unexpected error. (WS-009)"
        Just ws -> do
            runDB $ updateWhere [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey ws] [AssignmentWsOrder =. orderWs]
            updateWsOrder inputA u
updateWsOrder _ _ = return $ RepPlain "OK"


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - html widgets --------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


defaultHeader :: Text -> Widget
defaultHeader sessionUserId = do
    setTitle title
    addScriptRemote "http://code.jquery.com/jquery-2.1.3.min.js"
    toWidgetHead [hamlet| 
        $doctype 5
        <meta http-equiv="content-type" content="text/html; charset=UTF-8"> |]
    [whamlet|<script> var sessionUserId = "#{sessionUserId}"; |]
    [whamlet| ^{defaultCSS} |]
    [whamlet| <div ."titleClass" id="title">#{title} |]


tabRow :: String -> [Entity Workspace] -> String -> Widget
tabRow currentWs wsList role = do 
    [whamlet| ^{tabRowJS} |]
    [whamlet|
    <table id="tabTable">
        <tbody>
            <tr>
                <td id="tabTd">
                    <a ."tabClassArrow" id="tabLeft" onclick="previousTab();"><-
                    $forall Entity _ ws <- wsList
                        $if currentWs == unpack (workspaceName ws)
                             <a ."tabClassActive" href="/ws/#{workspaceName ws}">#{workspaceName ws}
                        $else
                            <a ."tabClass" href="/ws/#{workspaceName ws}">#{workspaceName ws}
                    <a ."tabClassArrow" id="tabRight" onclick="nextTab();">->
                <td>
                    <a id="logoutLink" href="/logout">Logout
                    <a id="myUserLink" href="/myuser">My Profile
                    <a id="newWsLink" href="/newws">New Workspace
                    $if role == "Admin"
                        <a id="newUserLink" href="/newuser">New User
                    <a id="homeLink" href="/">Home
|]


defaultFooter :: Widget
defaultFooter = do
    mmsg <- getMessage
    case mmsg of
        Nothing     -> [whamlet| <div ."footerClass" id="footer">&nbsp; |]
        Just msg    -> [whamlet| <div ."footerClass" id="footer">#{msg} |]


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ (\id pw -> User id pw "")
    <$> areq textField "UserId" Nothing
    <*> areq passwordField "Password" Nothing


userNewForm :: Widget
userNewForm = do [whamlet|
    <table>
        <tbody>
            <tr>
                <td>
                    <form method=post>
                        <div>
                            <label for="userId">UserId
                            <input type="text" name="userId" id="userId" required>
                        <div>
                            <label for="role">Role
                            <select name="role" id="role">
                                <option name="User" value="User">User
                                <option name="Admin" value="Admin">Admin
                        <div>
                            <input type="submit" ."btn" id="save" name="save" value="Save">
                            <input type="submit" ."btn" id="saveAndNew" name="saveAndNew" value="Save & New">
|]


userEditForm :: String -> String -> Widget
userEditForm userId role = do [whamlet|
    <table>
        <tbody>
            <tr>
                <td>
                    <form method=post>
                        <div>
                            <label for="userId">UserId
                            <input type="text" name="userId" id="userId" value="#{userId}" DISABLED>
                        <div>
                            <label for="role">Role
                            <select name="role" id="role">
                                $if role == "User"
                                    <option name="User" value="User" selected="selected">User
                                $else
                                    <option name="User" value="User">User
                                $if role == "Admin"
                                    <option name="Admin" value="Admin" selected="selected">Admin
                                $else
                                    <option name="Admin" value="Admin">Admin
                        <div>
                            <input type="submit" ."btn" id="saveUser" name="save" value="Update Role">
                <td>
                    <form method=get action=/user/#{userId}/pw>
                        <div><input style="visibility: hidden">
                        <div><input style="visibility: hidden">
                        <div>
                            <input type="submit" ."btnLong" id="resetPassword" name="resetPassword" value="Reset Password">
                <td>
                    <form method=get action=/user/#{userId}/delete>
                        <div><input style="visibility: hidden">
                        <div><input style="visibility: hidden">
                        <div>
                            <input type="submit" ."btnLong" id="deleteUser" name="deleteUser" value="Delete User">
|]


myUserForm :: String -> String -> Widget
myUserForm userId role = do [whamlet|
    <table>
        <tbody>
            <tr>
                <td>
                    <form>
                        <div>
                            <label for="userId">UserId
                            <input type="text" name="userId" id="userId" value="#{userId}" DISABLED>
                        <div>
                            <label for="role">Role
                            <input type="text" name="role" id="role" value="#{role}" DISABLED>
                        <div>
                            <input style="visibility: hidden">
                <td>
                    <form method=post>
                        <div>
                            <label ."longLabel" for="newPassword">New Password
                            <input type="password" name="newPassword" id="newPassword" required>
                        <div>
                            <label ."longLabel" for="newPasswordR">Repeat New Password
                            <input type="password" name="newPasswordR" id="newPasswordR" required>
                        <div>
                            <input type="submit" ."btnLong" id="setPassword" name="setPassword" value="Change Password">
|]


workspaceForm :: Html -> MForm Handler (FormResult Workspace, Widget)
workspaceForm = renderDivs $ (\name -> Workspace name "" "0" "")
    <$> areq shortTextField "Name" Nothing
    where
        shortTextField = check validateLength textField
        validateLength t
            | Data.Text.length t > 20 = Left ("The maximum length for a Workspace Name is 20 characters." :: Text)
            | otherwise     = Right t


workspaceWorkForm :: Workspace -> String -> Widget
workspaceWorkForm (Workspace name note status assignee) right = do 
    [whamlet| ^{workspaceJS} |]
    [whamlet| ^{tabIndentJS} |]
    [whamlet|
    <table id="workspaceTable">
        <tbody>
            <tr>
                <td>
                    <form method=post>
                        <input type="text" id="workspaceName" style="display: none;" value="#{name}">
                        <input type="text" id="workspaceStatus" style="display: none;" value="#{status}">
                        <input type="text" id="workspaceAssignee" style="display: none;" value="#{assignee}">
                        <textarea name="noteArea" id="noteArea">#{note}
                        <input type="text" id="right" style="display: none;" value="#{right}">
                        <input type="submit" ."btn" id="getSubmit" name="get" value="Get">
                        $if right /= "ReadOnly"
                            <input type="submit" ."btn" id="checkInSubmit" name="checkIn" value="Check In">
                            <input type="submit" ."btn" id="checkOutSubmit" name="checkOut" value="Check Out">
                        $if right == "Admin"
                            <input type="submit" ."adminBtn" id="deleteSumbit" name="delete" value="Delete">
                            <input type="submit" ."adminBtn" id="discardCheckOutSubmit" name="discardCheckOut" value="Discard CO">
|]


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - js widgets ----------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


newWsJS :: Widget
newWsJS = toWidget [julius|
    document.getElementById("hident2").setAttribute("maxlength", "20");
|]


tabRowJS :: Widget
tabRowJS = toWidget [julius|
    var tabs = $("#tabTd > a");
    var numberOfTabs = tabs.length;
    var indexActiveTab = 0;
    var isCmd = false;
    var isAlt = false;
    var cmdKey = "Meta";        // cmd key
    var altKey = "Alt";         // alt key
    var nextKey = "Down";  // down arrow
    var prevKey = "Up";    // up arrow
    
    $(document).keyup(function(event) {
        if(event.key == cmdKey) isCmd = false;
        if(event.key == altKey) isAlt = false;
    });
    $(document).keydown(function(event) {
        if(event.key == cmdKey) isCmd = true;
        if(event.key == altKey) isAlt = true;
        if(event.key == nextKey && isCmd == true && isAlt == true) {
            nextTab();
            return false;
        } else if(event.key == prevKey && isCmd == true && isAlt == true) {
            previousTab();
            return false;
        }
    });
    
    
    function hideTabs() {
    
        if(document.getElementsByClassName("tabClassActive").length > 0) {
            while(tabs[indexActiveTab].className != "tabClassActive" && tabs[indexActiveTab+1]) {
                indexActiveTab++;
            }
        }
        
        for(var i=1; i < numberOfTabs-1; i++) {
            if(i < indexActiveTab-4 || i > Math.max(indexActiveTab,5)) {
                tabs[i].style.display = "none";
            } else {
                tabs[i].style.display = "inline-block";
            }
        }
        
        if(indexActiveTab == numberOfTabs-2) {
            document.getElementById("tabRight").style.visibility = "hidden";
        } else {
            document.getElementById("tabRight").style.visibility = "visible";
        }
        if(indexActiveTab <= 1) {
            document.getElementById("tabLeft").style.visibility = "hidden";
        } else {
            document.getElementById("tabLeft").style.visibility = "visible";
        }
    }
    
    function nextTab() {
        if(indexActiveTab < numberOfTabs - 2) {
            indexActiveTab++;
            window.location = "/ws/" + tabs[indexActiveTab].innerHTML;
        }
    }
    
    function previousTab() {
        if(indexActiveTab > 1) {
            indexActiveTab--;
            window.location = "/ws/" + tabs[indexActiveTab].innerHTML;
        }
    }
    
    hideTabs();
|]


assignmentJS :: Widget
assignmentJS = toWidget [julius|
    var assignmentTable = document.getElementById("assignmentTable");
    if(assignmentTable) {
        listOfRights = assignmentTable.getElementsByTagName("select");
        if(listOfRights) {
            for(var i=0; i<listOfRights.length; i++) {
                listOfRights[i].onchange = function() {
                    try {
                        $.ajax ({
                            type        : "POST",
                            url         : "/user/" + encodeURIComponent(document.getElementById("userId").value) +
                                          "/assign/" + this.parentNode.previousSibling.previousSibling.innerHTML +
                                          "&" + this.value
                        }).done(function(data) {
                        }).fail(function(jqXHR, textStatus, errorThrown) {
                            alert("Error: " + jqXHR.status + " - " + errorThrown + ". ");
                        });
                    } catch (e) {
                        console.log("Error: " + e);
                    }
                }
            }
        }
    }
|]


workspaceJS :: Widget
workspaceJS = toWidget [julius| 
    var wsId = "";
    var right = "";
    var status = "";
    var assignee = "";
    var isCtrl = false;
    var ctrlKey = "Control" // ctrl key
    
    $(document).keyup(function(event) {
        if(event.key == ctrlKey) isCtrl = false;
    });
    $(document).keydown(function(event) {
        if(event.key == ctrlKey) isCtrl = true;
        if(event.which == 71 && isCtrl == true) {
            wsId = document.getElementById("workspaceName").value;
            status = document.getElementById("workspaceStatus").value;
            assignee = document.getElementById("workspaceAssignee").value;
        
            if(status == 1 && assignee == sessionUserId) {
                if(confirm("The file is currently checked out to you. All your changes are lost.")) {
                    getWsRest(wsId, "get");
                }
            } else {
                getWsRest(wsId, "get");
            }
            return false;
        } else if(event.which == 73 && isCtrl == true) {
            wsId = document.getElementById("workspaceName").value;
            postWsRest(wsId, "checkIn");
            return false;
        } else if(event.which == 79 && isCtrl == true) {
            wsId = document.getElementById("workspaceName").value;
            getWsRest(wsId, "checkOut");
            return false;
        } else if(event.which == 83 && isCtrl == true) {
            wsId = document.getElementById("workspaceName").value;
            postWsRest(wsId, "save");
            return false;
        }
    });
    
    function getWsRest(id, action) {
        $.ajax ({
            type        : "GET",
            url         : "/ws/rest/" + encodeURIComponent(id + "&" + action)
        }).done(function(data) {
            document.getElementById("footer").innerHTML = data.statusMsg;
        
            if(data.statusCode == "000") {
                document.getElementById("noteArea").value = data.workspace.note;
                
                if(action == "checkOut") {
                    document.getElementById("workspaceAssignee").value = sessionUserId;
                    document.getElementById("workspaceStatus").value = "1";
                    setNoteAreaEdit();
                }
            } else {
                alert(data.statusMsg);
            }
        }).fail(function(jqXHR, textStatus, errorThrown) {
            alert("Error: " + jqXHR.status + " - " + errorThrown + ". ");
        });
    }
    
    function postWsRest(id, action) {
        $.ajax ({
            type        : "POST",
            url         : "/ws/rest/" + encodeURIComponent(wsId+"&"+action+"&"+document.getElementById("noteArea").value)
        }).done(function(data) {
            document.getElementById("footer").innerHTML = data.statusMsg;
            
            if(data.statusCode == "000" && action == "checkIn") {
                document.getElementById("workspaceAssignee").value = "";
                document.getElementById("workspaceStatus").value = "0";
                setNoteAreaReadOnly();
            } else if(data.statusCode != "000") {
                alert(data.statusMsg);
            }
        }).fail(function(jqXHR, textStatus, errorThrown) {
            alert("Error: " + jqXHR.status + " - " + errorThrown + ". ");
        });
    }
    
    function setNoteAreaReadOnly() {
        document.getElementById("noteArea").setAttribute("readonly", "readonly");
        document.getElementById("noteArea").style.backgroundColor = "#e6e6e6";
        document.getElementById("noteArea").style.color = "#555555";
        
        var checkOutBtn = document.getElementById("checkOutSubmit");
        if(checkOutBtn) checkOutBtn.disabled = false;
        var checkInBtn = document.getElementById("checkInSubmit");
        if(checkInBtn) checkInBtn.disabled = true;   
    }
    
    function setNoteAreaEdit() {
        document.getElementById("noteArea").removeAttribute("readonly");
        document.getElementById("noteArea").style.backgroundColor = "#FFFFFF";
        document.getElementById("noteArea").style.color = "#000000";
        
        var checkOutBtn = document.getElementById("checkOutSubmit");
        if(checkOutBtn) checkOutBtn.disabled = true;
        var checkInBtn = document.getElementById("checkInSubmit");
        if(checkInBtn) checkInBtn.disabled = false;
    }
    
    window.onload = function() {
        right = document.getElementById("right").value;
        status = document.getElementById("workspaceStatus").value;
        assignee = document.getElementById("workspaceAssignee").value;
        
        if(right == "ReadOnly" || (status == 1 && assignee != sessionUserId) || status == 0) {
            setNoteAreaReadOnly();
        } else {
            setNoteAreaEdit();
        }
    }
|]


tabIndentJS :: Widget
tabIndentJS = toWidget [julius|
    $(window).load(function() {
        var isCtrlTab = false;
        var ctrlKeyTab = "Control" // ctrl key
        
        document.onkeyup = function(event) {
            if(event.key == ctrlKeyTab) isCtrlTab = false;
        }
        document.onkeydown = function(event) {
            if(event.key == ctrlKeyTab) isCtrlTab = true;
        }
        $("textarea").keydown(function(event) {
            if(event.key == "Tab" && !isCtrlTab) {
                var start = this.selectionStart;
                var end = this.selectionEnd;

                var target = event.target;
                var value = target.value;

                target.value = value.substring(0, start) + "\t" + value.substring(end);
                this.selectionStart = this.selectionEnd = start + 1;

                event.preventDefault();
            }
        });
    });
|]


workspaceOrderJS :: Widget
workspaceOrderJS = toWidget [julius|
    function moveUp() {
        $("#workspaceOrder option:selected").each( function() {
            var newPos = $("#workspaceOrder option").index(this) - 1;
            if (newPos > -1) {
                $("#workspaceOrder option").eq(newPos).before("<option value='"+$(this).val()+"' selected='selected'>"+$(this).text()+"</option>");
                $(this).remove();
            }
        });
    }
    
    function moveDown() {
        var countOptions = $("#workspaceOrder option").size();
        $("#workspaceOrder option:selected").each( function() {
            var newPos = $("#workspaceOrder option").index(this) + 1;
            if (newPos < countOptions) {
                $("#workspaceOrder option").eq(newPos).after("<option value='"+$(this).val()+"' selected='selected'>"+$(this).text()+"</option>");
                $(this).remove();
            }
        });
    }
    
    function saveWSOrder() {
        var i = 0;
        var wsOrder = "";
        
        $("#workspaceOrder option").each( function() {
            wsOrder += $(this).val() + "&" + i + "&";
            i++;
        });
        
        try {
            $.ajax ({
                type        : "POST",
                url         : "/ws/order/" + encodeURIComponent(wsOrder)
            }).done(function(data) {
                alert("New Order Saved.")
            }).fail(function(jqXHR, textStatus, errorThrown) {
                alert("Error: " + jqXHR.status + " - " + errorThrown + ". ");
            });
        } catch (e) {
            console.log("Error: " + e);
        }
    }
|]

{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - css widget ----------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


defaultCSS :: Widget
defaultCSS = toWidgetHead defaultCSSL


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - help functions ------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


queryWorkspaceAssignmentByUser :: Entity User -> Handler [(Entity Workspace, Entity Assignment)]
queryWorkspaceAssignmentByUser user = runDB $ rawSql queryWorkspaceAssignmentByUserStmt [toPersistValue $ entityKey user]


queryWorkspaceAssignmentByUserOuter :: Entity User -> Handler [(Entity Workspace, Maybe (Single Text))]
queryWorkspaceAssignmentByUserOuter user = runDB $ rawSql queryWorkspaceAssignmentByUserOuterStmt [toPersistValue $ entityKey user]


writeWSFile :: String -> String -> String -> IO ()
writeWSFile name note userid = do 
    time <- getClockTime
    appendFile ("log/" ++ name ++ ".log") ("*************** " ++ (show time) ++ " ***** " ++ userid ++ " ***************\n\n" ++ note ++ "\n\n**********************************************************************\n")
    writeFile ("log/" ++ name ++ ".txt") note


getText :: Maybe Textarea -> String
getText (Just text) = unpack $ unTextarea text
getText Nothing     = ""


isJust2 :: Maybe a -> Maybe b -> Bool
isJust2 Nothing Nothing = False
isJust2 _       _       = True


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - constant variables --------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


openConnectionCount :: Int
openConnectionCount = 20

title :: Html
title = "Shared Workspace"


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - main function -------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


main = do
    runStderrLoggingT $ withSqlitePool "workspace.db3" openConnectionCount $ \pool -> liftIO $ do
        runResourceT $ flip runSqlPool pool $ do
            runMigration migrateAll
            --insert $ User "Max" "Max" "Admin"
        warp 3001 $ WS pool