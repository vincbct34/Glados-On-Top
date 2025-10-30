{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Binary decoder for Ratatouille bytecode
-- Decodes bytecode from binary file format
-}

module Ratatouille.Bytecode.Decoder
  ( decodeBytecode,
    readBinaryFile,
    decodeInstruction,
    decodeValue,
  )
where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Ratatouille.Bytecode.Types

-- | Decode bytecode from binary format
decodeBytecode :: BL.ByteString -> Either String Bytecode
decodeBytecode bytes = 
  case runGetOrFail getBytecode bytes of
    Left (_, _, err) -> Left err
    Right (_, _, result) -> Right result

-- | Read bytecode from a binary file
readBinaryFile :: FilePath -> IO (Either String Bytecode)
readBinaryFile filepath = do
  bytes <- BL.readFile filepath
  return $ decodeBytecode bytes

-- | Main bytecode decoder
getBytecode :: Get Bytecode
getBytecode = do
  -- Check magic number
  magic <- getLazyByteString 4
  if magic /= BL.pack [0x52, 0x54, 0x42, 0x43]  -- "RTBC"
    then fail "Invalid magic number - not a Ratatouille bytecode file"
    else return ()
  
  -- Check version
  verMajor <- getWord8
  verMinor <- getWord8
  if verMajor /= 1 || verMinor /= 0
    then fail $ "Unsupported version: " ++ show verMajor ++ "." ++ show verMinor
    else return ()
  
  -- Get instruction count
  instrCount <- getWord32le
  
  -- Decode instructions
  sequence $ replicate (fromIntegral instrCount) decodeInstruction

-- | Decode a single instruction
decodeInstruction :: Get Instruction
decodeInstruction = do
  opcode <- getWord8
  case opcode of
    -- Stack operations
    0x01 -> do
      typeTag <- getWord8
      case typeTag of
        0x00 -> PUSH_INT <$> decodeInteger
        0x01 -> PUSH_FLOAT <$> getDoublele
        0x02 -> PUSH_STRING <$> decodeText
        0x03 -> PUSH_ATOM <$> decodeText
        _ -> fail $ "Unknown PUSH type tag: " ++ show typeTag
    
    0x02 -> PUSH_TUPLE . fromIntegral <$> getWord32le
    0x03 -> PUSH_ARRAY . fromIntegral <$> getWord32le
    0x04 -> return PUSH_UNIT
    
    -- Variable operations
    0x10 -> LOAD_VAR <$> decodeText
    0x11 -> STORE_VAR <$> decodeText
    0x12 -> LOAD_LOCAL <$> decodeText
    0x13 -> STORE_LOCAL <$> decodeText
    
    -- Array operations
    0x14 -> return INDEX
    0x15 -> return ARRAY_LENGTH
    
    -- Process state operations
    0x20 -> return INIT_STATE
    0x21 -> return GET_STATE
    0x22 -> return SET_STATE
    
    -- Arithmetic operations
    0x30 -> return ADD
    0x31 -> return SUB
    0x32 -> return MUL
    0x33 -> return DIV
    0x34 -> return CONCAT
    
    -- Increment/Decrement operations
    0x35 -> INC_VAR <$> decodeText
    0x36 -> DEC_VAR <$> decodeText
    0x37 -> INC_VAR_POST <$> decodeText
    0x38 -> DEC_VAR_POST <$> decodeText
    
    -- Comparison operations
    0x40 -> return CMP_EQ
    0x41 -> return CMP_NEQ
    0x42 -> return CMP_LT
    0x43 -> return CMP_GT
    0x44 -> return CMP_LTE
    0x45 -> return CMP_GTE
    
    -- Logical operations
    0x46 -> return LOGIC_AND
    0x47 -> return LOGIC_OR
    
    -- Value operations
    0x50 -> return PUSH_NONE
    0x51 -> PUSH_BOOL . (== 1) <$> getWord8
    0x52 -> GET_FIELD <$> decodeText
    
    -- Maybe/Either operations
    0x53 -> return PUSH_JUST
    0x54 -> return PUSH_LEFT
    0x55 -> return PUSH_RIGHT
    
    -- Actor model operations
    0x60 -> do
      name <- decodeText
      paramCount <- getWord32le
      params <- sequence $ replicate (fromIntegral paramCount) decodeText
      bodyCount <- getWord32le
      body <- sequence $ replicate (fromIntegral bodyCount) decodeInstruction
      return $ DEFINE_PROCESS name params body
    
    0x61 -> do
      name <- decodeText
      argCount <- getWord32le
      return $ CREATE_INSTANCE name (fromIntegral argCount)
    
    0x62 -> return SEND
    0x63 -> return WAIT_MESSAGE
    
    -- Pattern matching operations
    0x70 -> MATCH_ATOM <$> decodeText <*> (fromIntegral <$> getWord32le)
    0x71 -> MATCH_VAR <$> decodeText
    0x72 -> MATCH_TUPLE <$> (fromIntegral <$> getWord32le) <*> (fromIntegral <$> getWord32le)
    0x73 -> return MATCH_WILDCARD
    
    -- Process control
    0x80 -> return PROCESS_LOOP
    0x81 -> return SELF
    0x82 -> return EXIT_PROCESS
    
    -- Type casting operations
    0x90 -> STATIC_CAST <$> decodeText
    0x91 -> REINTERPRET_CAST <$> decodeText
    0x92 -> return CONST_CAST
    
    -- Control flow
    0xA0 -> JUMP . fromIntegral <$> getInt32le
    0xA1 -> JUMP_IF_FALSE . fromIntegral <$> getInt32le
    0xA2 -> LABEL <$> decodeText
    0xA3 -> CALL <$> decodeText
    0xA4 -> return RETURN
    0xA5 -> return PRINT
    0xFF -> return HALT
    
    _ -> fail $ "Unknown opcode: 0x" ++ showHex opcode

-- | Decode a Value
decodeValue :: Get Value
decodeValue = do
  tag <- getWord8
  case tag of
    0x00 -> VInt <$> decodeInteger
    0x01 -> VFloat <$> getDoublele
    0x02 -> VString <$> decodeText
    0x03 -> VAtom <$> decodeText
    0x04 -> do
      count <- getWord32le
      VTuple <$> sequence (replicate (fromIntegral count) decodeValue)
    0x05 -> do
      count <- getWord32le
      VArray <$> sequence (replicate (fromIntegral count) decodeValue)
    0x06 -> VPid . fromIntegral <$> getWord64le
    0x07 -> return VUnit
    0x08 -> return VNone
    0x09 -> VBool . (== 1) <$> getWord8
    0x0A -> VJust <$> decodeValue
    0x0B -> VLeft <$> decodeValue
    0x0C -> VRight <$> decodeValue
    _ -> fail $ "Unknown value tag: " ++ show tag

-- | Decode Text from length-prefixed UTF-8
decodeText :: Get Text
decodeText = do
  len <- getWord32le
  bytes <- getByteString (fromIntegral len)
  return $ TE.decodeUtf8 bytes

-- | Decode Integer (variable-length encoding)
decodeInteger :: Get Integer
decodeInteger = do
  sizeAndSign <- getWord8
  if sizeAndSign == 0x00
    then return 0
    else do
      let isNegative = sizeAndSign >= 0x80
          size = if isNegative then sizeAndSign - 0x80 else sizeAndSign
      bytes <- sequence $ replicate (fromIntegral size) getWord8
      let value = bytesToInteger bytes
      return $ if isNegative then -value else value

-- | Convert bytes to Integer (little-endian)
bytesToInteger :: [Word8] -> Integer
bytesToInteger = foldr (\b acc -> acc * 256 + fromIntegral b) 0 . reverse

-- | Show hexadecimal value
showHex :: Word8 -> String
showHex w = let (q, r) = w `divMod` 16
                toHex n = if n < 10 then toEnum (fromEnum '0' + fromIntegral n)
                                    else toEnum (fromEnum 'A' + fromIntegral n - 10)
            in [toHex q, toHex r]
