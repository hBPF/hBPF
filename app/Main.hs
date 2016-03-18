{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude hiding ((&&), (||)) -- So we can use hBPF (&&)
import HBPF (toString, compile, (&&), (||))
import IpTcpDns (isIpPacket, isTcpPacket, isUdpPacket, isDstPort)

main :: IO ()
main = do
  let filter = compile $
                isIpPacket
            && (isUdpPacket || isTcpPacket)
            && (isDstPort 53)
  putStrLn $ toString filter
