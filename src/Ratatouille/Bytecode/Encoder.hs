{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Binary encoder for Ratatouille bytecode
-- Encodes bytecode instructions into a binary file format
-}

module Ratatouille.Bytecode.Encoder
  ( encodeBytecode,
    writeBinaryFile,
    encodeInstruction,
    encodeValue,
  )
where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Ratatouille.Bytecode.Types

-- =============================================================================
-- BINARY FILE FORMAT
-- =============================================================================
-- Magic number: "RTBC" (Ratatouille Bytecode) - 4 bytes
-- Version: 1.0 - 2 bytes (major.minor)
-- Instruction count: 4 bytes (uint32)
-- Instructions: variable length
-- =============================================================================

magicNumber :: BL.ByteString
magicNumber = BL.pack [0x52, 0x54, 0x42, 0x43]  -- "RTBC"

versionMajor :: Word8
versionMajor = 1

versionMinor :: Word8
versionMinor = 0

-- | Encode bytecode to binary format
encodeBytecode :: Bytecode -> BL.ByteString
encodeBytecode instructions = runPut $ do
  -- Header
  putLazyByteString magicNumber
  putWord8 versionMajor
  putWord8 versionMinor
  putWord32le (fromIntegral $ length instructions)
  
  -- Instructions
  mapM_ encodeInstruction instructions

-- | Write bytecode to a binary file
writeBinaryFile :: FilePath -> Bytecode -> IO ()
writeBinaryFile filepath bytecode = 
  BL.writeFile filepath (encodeBytecode bytecode)

-- | Encode a single instruction
-- Format: opcode (1 byte) + operands (variable)
encodeInstruction :: Instruction -> Put
encodeInstruction instr = case instr of
  -- Stack operations
  PUSH_INT n -> putWord8 0x01 >> putWord8 0x00 >> encodeInteger n
  PUSH_FLOAT d -> putWord8 0x01 >> putWord8 0x01 >> putDoublele d
  PUSH_STRING t -> putWord8 0x01 >> putWord8 0x02 >> encodeText t
  PUSH_ATOM t -> putWord8 0x01 >> putWord8 0x03 >> encodeText t
  PUSH_TUPLE size -> putWord8 0x02 >> putWord32le (fromIntegral size)
  PUSH_ARRAY size -> putWord8 0x03 >> putWord32le (fromIntegral size)
  
  PUSH_UNIT -> putWord8 0x04
  
  -- Variable operations
  LOAD_VAR name -> putWord8 0x10 >> encodeText name
  STORE_VAR name -> putWord8 0x11 >> encodeText name
  LOAD_LOCAL name -> putWord8 0x12 >> encodeText name
  STORE_LOCAL name -> putWord8 0x13 >> encodeText name
  
  -- Array operations
  INDEX -> putWord8 0x14
  ARRAY_LENGTH -> putWord8 0x15
  
  -- Process state operations
  INIT_STATE -> putWord8 0x20
  GET_STATE -> putWord8 0x21
  SET_STATE -> putWord8 0x22
  
  -- Arithmetic operations
  ADD -> putWord8 0x30
  SUB -> putWord8 0x31
  MUL -> putWord8 0x32
  DIV -> putWord8 0x33
  CONCAT -> putWord8 0x34
  
  -- Increment/Decrement operations
  INC_VAR name -> putWord8 0x35 >> encodeText name
  DEC_VAR name -> putWord8 0x36 >> encodeText name
  INC_VAR_POST name -> putWord8 0x37 >> encodeText name
  DEC_VAR_POST name -> putWord8 0x38 >> encodeText name
  
  -- Comparison operations
  CMP_EQ -> putWord8 0x40
  CMP_NEQ -> putWord8 0x41
  CMP_LT -> putWord8 0x42
  CMP_GT -> putWord8 0x43
  CMP_LTE -> putWord8 0x44
  CMP_GTE -> putWord8 0x45
  
  -- Logical operations
  LOGIC_AND -> putWord8 0x46
  LOGIC_OR -> putWord8 0x47
  LOGIC_NOT -> putWord8 0x48
  
  -- Unary operations
  NEGATE -> putWord8 0x49
  
  -- Value operations
  PUSH_NONE -> putWord8 0x50
  PUSH_BOOL b -> putWord8 0x51 >> putWord8 (if b then 1 else 0)
  GET_FIELD name -> putWord8 0x52 >> encodeText name
  
  -- Maybe/Either operations
  PUSH_JUST -> putWord8 0x53
  PUSH_LEFT -> putWord8 0x54
  PUSH_RIGHT -> putWord8 0x55
  MAYBE_BIND funcName -> putWord8 0x56 >> encodeText funcName
  EITHER_BIND funcName -> putWord8 0x57 >> encodeText funcName
  
  -- Actor model operations
  DEFINE_PROCESS name params body -> do
    putWord8 0x60
    encodeText name
    putWord32le (fromIntegral $ length params)
    mapM_ encodeText params
    putWord32le (fromIntegral $ length body)
    mapM_ encodeInstruction body
  
  CREATE_INSTANCE name argCount -> do
    putWord8 0x61
    encodeText name
    putWord32le (fromIntegral argCount)
  
  SEND -> putWord8 0x62
  WAIT_MESSAGE -> putWord8 0x63
  
  -- Pattern matching operations
  MATCH_ATOM atom offset -> 
    putWord8 0x70 >> encodeText atom >> putWord32le (fromIntegral offset)
  MATCH_VAR name -> putWord8 0x71 >> encodeText name
  MATCH_TUPLE size offset -> 
    putWord8 0x72 >> putWord32le (fromIntegral size) >> putWord32le (fromIntegral offset)
  
  MATCH_WILDCARD -> putWord8 0x73
  
  -- Process control
  PROCESS_LOOP -> putWord8 0x80
  SELF -> putWord8 0x81
  EXIT_PROCESS -> putWord8 0x82
  
  -- Type casting operations
  STATIC_CAST typeName -> putWord8 0x90 >> encodeText typeName
  REINTERPRET_CAST typeName -> putWord8 0x91 >> encodeText typeName
  
  CONST_CAST -> putWord8 0x92
  
  -- Control flow
  JUMP offset -> putWord8 0xA0 >> putInt32le (fromIntegral offset)
  JUMP_IF_FALSE offset -> putWord8 0xA1 >> putInt32le (fromIntegral offset)
  LABEL name -> putWord8 0xA2 >> encodeText name
  CALL name -> putWord8 0xA3 >> encodeText name
  
  RETURN -> putWord8 0xA4
  PRINT -> putWord8 0xA5
  HALT -> putWord8 0xFF

-- | Encode a Value (for future use in data sections)
encodeValue :: Value -> Put
encodeValue val = case val of
  VInt n -> putWord8 0x00 >> encodeInteger n
  VFloat d -> putWord8 0x01 >> putDoublele d
  VString t -> putWord8 0x02 >> encodeText t
  VAtom t -> putWord8 0x03 >> encodeText t
  
  VTuple vals -> do
    putWord8 0x04  -- Tag: tuple
    putWord32le (fromIntegral $ length vals)
    mapM_ encodeValue vals
  
  VArray vals -> do
    putWord8 0x05  -- Tag: array
    putWord32le (fromIntegral $ length vals)
    mapM_ encodeValue vals
  
  VPid pid -> putWord8 0x06 >> putWord64le (fromIntegral pid)
  VUnit -> putWord8 0x07
  VNone -> putWord8 0x08
  VBool b -> putWord8 0x09 >> putWord8 (if b then 1 else 0)
  VJust v -> putWord8 0x0A >> encodeValue v
  VLeft v -> putWord8 0x0B >> encodeValue v
  VRight v -> putWord8 0x0C >> encodeValue v

-- | Encode Text as length-prefixed UTF-8
encodeText :: Text -> Put
encodeText t = 
  let bytes = TE.encodeUtf8 t
  in putWord32le (fromIntegral $ BL.length $ BL.fromStrict bytes) >> putByteString bytes

-- | Encode Integer (variable-length encoding)
-- Uses a simple format: 1 byte for sign + size, then the bytes
encodeInteger :: Integer -> Put
encodeInteger n
  | n == 0 = putWord8 0x00
  | n > 0 = 
      let bytes = integerToBytes n
      in putWord8 (fromIntegral $ length bytes) >> mapM_ putWord8 bytes
  | otherwise = 
      let bytes = integerToBytes (abs n)
      in putWord8 (fromIntegral $ length bytes + 0x80) >> mapM_ putWord8 bytes

-- | Convert Integer to bytes (little-endian)
integerToBytes :: Integer -> [Word8]
integerToBytes 0 = []
integerToBytes n = fromIntegral (n `mod` 256) : integerToBytes (n `div` 256)
