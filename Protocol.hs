module Protocol
( Message (..)
, RequestJoin (..)
, RequestLeave (..)
, RequestDisconnect (..)
, ChatMsg (..)
, createConfirmJoin
, createConfirmLeave
, createChatMsg
, errorMsg
, parseMsgString
)

where

import Data.List.Split
import Control.Exception
import Data.Char



data RequestJoin = RequestJoin { joinChatRmName  :: String
                                , joinClientIp   :: String
                                , joinPort       :: Int
                                , joinClientName :: String }

data RequestLeave = RequestLeave { leaveChatRmId :: Int
                                  , leaveClientId :: Int
                                  , leaveClientName :: String }

data RequestDisconnect = RequestDisconnect { discClientIp :: String
                                            , discPort    :: Int
                                            , discClientName :: String }

data ChatMsg = ChatMsg { chatChatRmId :: Int
                        , chatClientId :: Int
                        , chatClientName :: String
                        , chatMessage :: String }

data Message a b c d =  Join a | Leave b | Disconnect c | Chat d 

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

createChatMsg :: String -> String -> String -> String
createChatMsg chatRmId clientName msg = 
    "CHAT: "    ++ chatRmId ++ "\n\
    \CLIENT_NAME: " ++ clientName    ++ "\n\
    \MESSAGE: " ++ msg ++ "\n\n"

errorMsg :: String -> String -> String
errorMsg code msg =
    "ERROR_CODE: "          ++ code ++ "\n\
    \ERROR_DESCRIPTION: "    ++ msg  ++ "\n\n"

parseMsgString :: String -> Maybe (Message  RequestJoin RequestLeave RequestDisconnect ChatMsg)
parseMsgString msgString 
    | (keys == joinKeys) = validateRequestJoin values
    | (keys == leaveKeys) = validateRequestLeave values
    | (keys == disconnectKeys) = validateRequestDisconnect values
    | (keys == chatKeys) = validateChatMsg values
    | otherwise = Nothing
    where splitUp = foldl (\ a str -> a ++ (splitOn ":" str)) [] (lines msgString)
          trimmed = filter (/= "") $ map (reverse . dropWhile isSpace . reverse . dropWhile isSpace) splitUp
          keys = stride 2 trimmed
          values = stride 2 $ drop 1 trimmed

validateRequestJoin :: [String] -> Maybe (Message RequestJoin RequestLeave RequestDisconnect ChatMsg)
validateRequestJoin values
    | length values == length joinKeys = 
        let l = zipWith validateType values joinTypes in
        if and l then Just (Join (RequestJoin (values!!0) (values!!1) (read $ values!!2) (values!!3)))
        else Nothing

validateRequestLeave :: [String] -> Maybe (Message RequestJoin RequestLeave RequestDisconnect ChatMsg)
validateRequestLeave values
    | length values == length leaveKeys = 
        let l = zipWith validateType values leaveTypes in
        if and l then Just (Leave (RequestLeave (read $ values!!0) (read $ values!!1) (values!!2)))
        else Nothing

validateRequestDisconnect :: [String] -> Maybe (Message RequestJoin RequestLeave RequestDisconnect ChatMsg)
validateRequestDisconnect values
    | length values == length disconnectKeys = 
        let l = zipWith validateType values disconnectTypes in
        if and l then Just (Disconnect (RequestDisconnect (values!!0) (read $ values!!1) (values!!2)))
        else Nothing
        
validateChatMsg :: [String] -> Maybe (Message RequestJoin RequestLeave RequestDisconnect ChatMsg)
validateChatMsg values
    | length values == length chatKeys = 
        let l = zipWith validateType values chatTypes in
        if and l then Just (Chat (ChatMsg (read $ values!!0) (read $ values!!1) (values!!2) (values!!3)))
        else Nothing

stride :: Int -> [a] -> [a]
stride _ [] = []
stride n (x:xs) = x : stride n (drop (n-1) xs)

validateType :: String -> String -> Bool
validateType value ty
    |ty == "str" = True
    |ty == "int" = 
        let a = reads value :: [(Int, String)] in 
        case a of
            [] -> False
            _ -> True
    |otherwise = False

testString = "JOIN_CHATROOM: chat\nCLIENT_IP:   0   \nPORT:0   \nCLIENT_NAME: Mr Curry  \n\n"

test :: String -> String
test str = case (parseMsgString str) of
    Just (Join a) -> "join"
    Just (Leave a) -> "leave"
    Just (Disconnect a) -> "disconnect"
    Just (Chat a) -> "chat"
    _ -> "ERROR"

        
