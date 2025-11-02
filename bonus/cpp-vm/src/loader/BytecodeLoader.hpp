/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** BytecodeLoader Class Definition
*/

#pragma once

#include <array>
#include <expected>
#include <filesystem>
#include <string>
#include <vector>

#include "common/ErrorCode.hpp"

namespace rat
{
    // Bytecode file format constants
    constexpr std::array<std::byte, 4> MAGIC_NUMBER = {std::byte{'R'}, std::byte{'T'}, std::byte{'B'}, std::byte{'C'}};
    constexpr uint8_t VERSION_MAJOR = 1;
    constexpr uint8_t VERSION_MINOR = 0;
    constexpr size_t HEADER_SIZE = 6; // Magic (4) + Version (2)

    /**
     * @class BytecodeLoader
     * @brief Responsible for loading and validating bytecode from files.
     * Uses modern C++23 features: std::expected for error handling, ranges/views for processing,
     * std::bit_cast for endianness if needed, cache-aligned structures.
     * Avoids heap allocation by using stack-based buffers where possible.
     * @author Robin Toillon
     */
    class alignas(64) BytecodeLoader {
        public:
            BytecodeLoader(void)= default;
            ~BytecodeLoader(void)= default;

            [[nodiscard]]
            auto loadFromFile(const std::filesystem::path &filePath)
                -> std::expected<std::vector<std::byte>, VMError>;

            [[nodiscard]]
            auto validateHeader(const std::vector<std::byte> &buffer)
                -> std::expected<void, VMError>;

        private:
            [[nodiscard]]
            auto decodeVarInt(std::vector<std::byte>::iterator &it,
                              std::vector<std::byte>::iterator end)
                -> std::expected<int64_t, VMError>;

            [[nodiscard]]
            auto decodeString(std::vector<std::byte>::iterator &it,
                              std::vector<std::byte>::iterator end)
                -> std::expected<std::string, VMError>;
    };
}
