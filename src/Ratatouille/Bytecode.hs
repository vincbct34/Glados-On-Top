{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Bytecode module - re-exports all bytecode-related functionality
-- This module provides a unified interface for bytecode operations:
--   - Types: Instruction, Value, Bytecode
--   - Compiler: AST to bytecode compilation
--   - Encoder: bytecode to binary file encoding
-}

module Ratatouille.Bytecode
  ( -- * Types
    module Ratatouille.Bytecode.Types,
    -- * Compiler
    module Ratatouille.Bytecode.Compiler,
    -- * Encoder
    module Ratatouille.Bytecode.Encoder,
    -- * Decoder
    module Ratatouille.Bytecode.Decoder,
  )
where

import Ratatouille.Bytecode.Compiler
import Ratatouille.Bytecode.Decoder
import Ratatouille.Bytecode.Encoder
import Ratatouille.Bytecode.Types
