module Protocol
( Message (..)
, createConfirmJoin
, createConfirmLeave
, createChatMsg
, errorMsg
, parseMessageString
)

where

import Data.List.Split
import Control.Exception



--data RequestJoin = RequestJoin { joinChatRmName  :: String
 --                               , joinClientIp   :: String
 --                               , joinPort       :: Int
 --                               , joinClientName :: String }

--data RequestLeave = RequestLeave { leaveChatRmId :: Int
--                                  , leaveClientId :: Int
--                                  , leaveClientName :: String }

--data RequestDisconnect = RequestDisconnect { discClientIp :: String
--                                            , discPort    :: Int
--                                            , discClientName :: String }

--data ChatMsg = ChatMsg { chatChatRmId :: Int
--                        , chatClientId :: Int
--                        , chatClientName :: String
--                        , chatMessage :: String }

data Message = RequestJoin String String Int String | RequestLeave Int Int String | RequestDisconnect String Int String | ChatMsg Int Int String String

joinKeys =  ["JOIN_CHATROOM", "CLIENT_IP", "PORT", "CLIENT_NAME"]
joinTypes = ["str"          , "str"      , "int" , "str"        ]

leaveKeys =  ["LEAVE_CHATROOM", "JOIN_ID", "CLIENT_NAME"]
leaveTypes = ["int"           , "int"    , "str"        ]

disconnectKeys =  ["DISCONNECT", "PORT", "CLIENT_NAME"]
disconnectTypes = ["str"       , "int" , "str"        ]

chatKeys =  ["CHAT", "JOIN_ID", "CLIENT_NAME", "MESSAGE"]
chatTypes = ["int" , "int"    , "str"        , "str"    ]

--requestJoin :: String -> String -> String -> String -> String
--requestJoin chatRmName, ip, port, clientName = 
--    "JOIN_CHATROOM: "   ++ chatRmName   ++ "\n\
--    CLIENT_IP: "        ++ ip           ++ "\n\
--    PORT: "             ++ port         ++ "\n\
--    CLIENT_NAME: "      ++ name         ++ "\n\n"

createConfirmJoin :: String -> String -> String -> String -> String -> String
createConfirmJoin chatRmName ip port chatRmId clientId =
    "JOINED_CHATROOM: " ++ chatRmName   ++ "\n\
    \SERVER_IP: "        ++ ip           ++ "\n\
    \PORT: "             ++ port         ++ "\n\
    \ROOM_REF: "         ++ chatRmId     ++ "\n\
    \JOIN_ID: "          ++ clientId     ++ "\n\n"

createConfirmLeave :: String -> String -> String 
createConfirmLeave chatRmId clientId = 
    "LEFT_CHATROOM: "   ++ chatRmId ++ "\n\
    \JOIN_ID: "          ++ clientId ++ "\n\n"

errorMsg :: String -> String -> String
errorMsg code msg =
    "ERROR_CODE: "          ++ code ++ "\n\
    \ERROR_DESCRIPTION: "    ++ msg  ++ "\n\n"

parseMsgString :: String -> Maybe Message
parseMsgString msgString 
    | keys == joinKeys = validateRequestJoin values
    | keys == leaveKeys = validateRequestLeave values
    | keys == disconnectKeys = validateRequestDisconnect values
    | keys == chatKeys = validateChatMsg values
    | otherwise = Nothing
    where splitUp = (splitOn ":" . lines msgString)
          trimmed = map (dropWhileEnd isSpace . dropWhile isSpace) splitUp
          keys = stride 2 trimmed
          values = stride 2 $ drop 1 trimmed

validateRequestJoin :: [String] -> Maybe RequestJoin
validateRequestJoin values
    | length values == length joinKeys = 
        let l = zipWith validateType values joinTypes in
        if and l then RequestJoin values!!0 values!!1 (read values!!2) values!!3
        else Nothing

validateRequestLeave :: [String] -> Maybe RequestLeave
validateRequestLeave values
    | length values == length leaveKeys = 
        let l = zipWith validateType values leaveTypes in
        if and l then RequestLeave (read values!!0) (read values!!1) values!!2
        else Nothing

validateRequestDisconnect :: [String] -> Maybe RequestDisconnect
validateRequestDisconnect values
    | length values == length disconnectKeys = 
        let l = zipWith validateType values disconnectTypes in
        if and l then RequestDisconnect values!!0 (read values!!1) values!!2
        else Nothing
        
validateChatMsg :: [String] -> Maybe ChatMsg
validateChatMsg values
    | length values == length chatKeys = 
        let l = zipWith validateType values chatTypes in
        if and l then RequestLeave (read values!!0) (read values!!1) values!!2 values!!3
        else Nothing

stride :: Int -> [a] -> [a]
stride _ [] = []
stride n (x:xs) = x : stride n (drop (n-1) xs)

validateType :: String -> String -> Bool
validateType value ty
    |ty == "str" = True
    |ty == "int" = 
        let a = (try $ evaluate $ (read value) :: Int) in 
        case a of
            Left e -> False
            Right v -> True
    |otherwise = False
        
