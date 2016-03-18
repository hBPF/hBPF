{-# LANGUAGE OverloadedStrings #-} -- Label has an instance of IsString, so we can use String literals for labels
-- |
-- Module      : IpTcpDns
-- Description : Exported API for HBPF
-- Copyright   : (c) Matt Denton and Andrew Duffy, 2016
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Example module using the interface provided by HBPF
-- to create a set of composable filters for IP/TCP/UDP
-- port matching.
--
module IpTcpDns
  (
    isIpPacket
  , isTcpPacket
  , isArpPacket
  , isUdpPacket
  , isSrcPort
  , isDstPort
  , isPort
  ) where

import Data.Monoid ((<>)) -- Using mappend, we can chain instructions together with a clean syntax
import HBPF

-- * Constants for our filters

ethertype_off = 12 :: Offset -- ^ Offset has an instance of Num (thanks to -XGeneralizedNewtypeDeriving)
transport_off = 23 :: Offset

ethertype_ipv4 = 0x800 :: Literal -- ^ ... and so does Literal
ethertype_arp  = 0x806 :: Literal

transport_udp = 0x11 :: Literal
transport_tcp = 0x06 :: Literal

-- | Filter that checks if is an IP packet.
isIpPacket :: Filter
isIpPacket = newFilter $
                 ldx (LoadAddrLiteral 0)
              <> stx (StoreAddrIndexedRegister M15)
              <> ldh (LoadAddrByteOff ethertype_off)
              <> jneq (ConditionalJmpAddrLiteralTrue ethertype_ipv4 "label1")
              <> ldx (LoadAddrLiteral 65535)
              <> stx (StoreAddrIndexedRegister M15)
              <> label "label1"

isArpPacket :: Filter
isArpPacket = newFilter $
                 ldx (LoadAddrLiteral 0)
              <> stx (StoreAddrIndexedRegister M15)
              <> ldh (LoadAddrByteOff ethertype_off)
              <> jneq (ConditionalJmpAddrLiteralTrue (ethertype_arp) "label1")
              <> ldx (LoadAddrLiteral 65535)
              <> stx (StoreAddrIndexedRegister M15)
              <> label "label1"


isUdpPacket :: Filter
isUdpPacket = newFilter $
                 ldx (LoadAddrLiteral 0)
              <> stx (StoreAddrIndexedRegister M15)
              <> ldh (LoadAddrByteOff transport_off)
              <> jneq (ConditionalJmpAddrLiteralTrue (transport_udp) "label1")
              <> ldx (LoadAddrLiteral 65535)
              <> stx (StoreAddrIndexedRegister M15)
              <> label "label1"

isTcpPacket :: Filter
isTcpPacket = newFilter $
                 ldx (LoadAddrLiteral 0)
              <> stx (StoreAddrIndexedRegister M15)
              <> ldh (LoadAddrByteOff transport_off)
              <> jneq (ConditionalJmpAddrLiteralTrue (transport_tcp) "label1")
              <> ldx (LoadAddrLiteral 65535)
              <> stx (StoreAddrIndexedRegister M15)
              <> label "label1"

ipPacketLengthField = Offset 14
sourcePortOffset = Offset 14
destPortOffset = Offset 16

isPort :: Int -> Filter
isPort port' = newFilter $
                 ldx (LoadAddrLiteral 0)
              <> stx (StoreAddrIndexedRegister M15)
              <> ldxb (LoadAddrNibble ipPacketLengthField) -- skip IP header
              <> ldh (LoadAddrOffFromX sourcePortOffset)
              <> jneq (ConditionalJmpAddrLiteralTrue port "label1")
              <> ldh (LoadAddrOffFromX destPortOffset)
              <> jneq (ConditionalJmpAddrLiteralTrue port "label1")
              <> ldx (LoadAddrLiteral 65535)
              <> stx (StoreAddrIndexedRegister M15)
              <> label "label1"
              where port = fromIntegral port'

isSrcPort :: Int -> Filter
isSrcPort port' = newFilter $
                 ldx (LoadAddrLiteral 0)
              <> stx (StoreAddrIndexedRegister M15)
              <> ldxb (LoadAddrNibble ipPacketLengthField) -- skip IP header
              <> ldh (LoadAddrOffFromX sourcePortOffset)
              <> jneq (ConditionalJmpAddrLiteralTrue port "label1")
              <> ldx (LoadAddrLiteral 65535)
              <> stx (StoreAddrIndexedRegister M15)
              <> label "label1"
              where port = fromIntegral port'

isDstPort :: Int -> Filter
isDstPort port' = newFilter $
                 ldx (LoadAddrLiteral 0)
              <> stx (StoreAddrIndexedRegister M15)
              <> ldxb (LoadAddrNibble ipPacketLengthField) -- skip IP header
              <> ldh (LoadAddrOffFromX destPortOffset)
              <> jneq (ConditionalJmpAddrLiteralTrue port "label1")
              <> ldx (LoadAddrLiteral 65535)
              <> stx (StoreAddrIndexedRegister M15)
              <> label "label1"
              where port = fromIntegral port'

