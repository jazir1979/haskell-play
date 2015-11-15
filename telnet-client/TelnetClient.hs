module Main where

import System.IO
import Control.Concurrent
import Data.Char
import Network.Socket
import System.Environment(getArgs)

telnetIAC :: Int
telnetIAC = 255

telnetWONT :: Int
telnetWONT = 252

newtype Credentials = Credential (String, String)

password :: Handle -> String -> IO ()
password h pwd = do
	char <- hGetChar h
	case char of 
		':' -> do hPutStrLn h pwd
		          hFlush h
		otherwise -> password h pwd
	
	
login :: Handle -> Credentials -> IO ()
login h c@(Credential (user, pwd)) = do
	hPutStrLn h user
	hFlush h
	password h pwd
	putStrLn "Logged in"
	
	
declineHandshakeOption :: Handle -> Credentials -> IO ()
declineHandshakeOption h c = do
	ignoreChar <- hGetChar h
	optionChar <- hGetChar h
	hPutChar h $ chr telnetIAC
	hPutChar h $ chr telnetWONT
	hPutChar h $ optionChar
	hFlush h
	handshakeAndLogin h c


handleHandshakeInput :: Handle -> Char -> Credentials -> IO()
handleHandshakeInput h char credentials
	| ord char == telnetIAC		= declineHandshakeOption h credentials
	| char == ':'				= login h credentials
	| otherwise					= handshakeAndLogin h credentials


handshakeAndLogin :: Handle -> Credentials -> IO ()
handshakeAndLogin h credentials = do
	char <- hGetChar h
	handleHandshakeInput h char credentials


main :: IO ()
main = withSocketsDo $ do
	args <- getArgs
	addrinfos <- getAddrInfo Nothing (Just (args !! 0)) (Just "23")
	let serveraddr = head addrinfos
	sock <- socket (addrFamily serveraddr) Stream defaultProtocol
	connect sock (addrAddress serveraddr)
	putStrLn "Connected"
	h <- socketToHandle sock ReadWriteMode
	hSetBuffering h (BlockBuffering Nothing)
	handshakeAndLogin h $ Credential (args !! 1, args !! 2)
	hPutStrLn h (args !! 3)
	hFlush h
	putStrLn "Command sent"
	threadDelay 1000000
