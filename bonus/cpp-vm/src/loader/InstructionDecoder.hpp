/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** InstructionDecoder Class Definition
*/

#pragma once

#include <array>
#include <expected>
#include <string>
#include <vector>

#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"

namespace rat
{
    /**
     * @class InstructionDecoder
     * @brief Decodes bytecode buffer into Instructions vector
     *
     * Implements high-performance bytecode decoding using a jump table
     * for O(1) instruction lookup instead of switch statements.
     * Cache-aligned to 64 bytes for optimal memory access patterns.
     *
     * @details
     * The decoder uses function pointer tables to dispatch decoding
     * based on opcode. Pattern matching instructions decode additional
     * offset parameters for control flow. All methods return
     * std::expected for comprehensive error handling.
     *
     * @author Robin Toillon
     */
    class alignas(64) InstructionDecoder {
        public:
            /**
             * @brief Constructs decoder and initializes jump table
             */
            InstructionDecoder(void);

            /**
             * @brief Default destructor
             */
            ~InstructionDecoder() = default;

            /**
             * @brief Decodes bytecode buffer into instructions
             *
             * @param buffer Bytecode buffer with header and instructions
             * @return Expected vector of instructions or error
             *
             * @note Buffer must have 6-byte header (magic + version)
             */
            [[nodiscard]]
            auto decode(const std::vector<std::byte> &buffer)
                -> std::expected<std::vector<Instruction>, VMError>;

        private:
            using ByteIterator = std::vector<std::byte>::const_iterator;
            using DecoderFunc =
                std::expected<Instruction, VMError>
                (InstructionDecoder::*)(ByteIterator &, ByteIterator);

            static constexpr size_t MAX_OPCODE = 72;
            std::array<DecoderFunc, MAX_OPCODE> _decoderTable;

            /**
             * @brief Decodes single instruction from bytecode stream
             *
             * @param it Iterator to current position
             * @param end Iterator to end of buffer
             * @return Expected instruction or error
             */
            [[nodiscard]]
            auto decodeInstruction(ByteIterator &it, ByteIterator end)
                -> std::expected<Instruction, VMError>;

            /**
             * @brief Decodes variable-length integer
             *
             * @param it Iterator to current position
             * @param end Iterator to end of buffer
             * @return Expected int64_t value or error
             */
            [[nodiscard]]
            auto decodeVarInt(ByteIterator &it, ByteIterator end)
                -> std::expected<int64_t, VMError>;

            /**
             * @brief Decodes length-prefixed string
             *
             * @param it Iterator to current position
             * @param end Iterator to end of buffer
             * @return Expected string or error
             */
            [[nodiscard]]
            auto decodeString(ByteIterator &it, ByteIterator end)
                -> std::expected<std::string, VMError>;

            /**
             * @brief Decodes instruction with string argument
             *
             * @tparam InstrType Instruction type to construct
             * @param it Iterator to current position
             * @param end Iterator to end of buffer
             * @return Expected instruction or error
             */
            template<typename InstrType>
            [[nodiscard]]
            auto decodeStringInstruction(ByteIterator &it,
                                        ByteIterator end)
                -> std::expected<Instruction, VMError>;

            /**
             * @brief Decodes instruction with varint argument
             *
             * @tparam InstrType Instruction type to construct
             * @param it Iterator to current position
             * @param end Iterator to end of buffer
             * @return Expected instruction or error
             */
            template<typename InstrType>
            [[nodiscard]]
            auto decodeVarIntInstruction(ByteIterator &it,
                                        ByteIterator end)
                -> std::expected<Instruction, VMError>;

            /**
             * @brief Decodes instruction with no arguments
             *
             * @tparam InstrType Instruction type to construct
             * @param it Iterator (unused)
             * @param end Iterator (unused)
             * @return Expected instruction or error
             */
            template<typename InstrType>
            [[nodiscard]]
            auto decodeNoArgInstruction(ByteIterator &, ByteIterator)
                -> std::expected<Instruction, VMError>;

            /**
             * @brief Initializes jump table with decoder functions
             */
            void initializeDecoderTable(void);
    };
}

