/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** BytecodeLoader Class Implementation
*/

#include "BytecodeLoader.hpp"
#include <algorithm>
#include <expected>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>
#include "common/ErrorCode.hpp"

namespace rat
{
    auto BytecodeLoader::loadFromFile(const std::filesystem::path& filePath)
        -> std::expected<std::vector<std::byte>, VMError> {
        std::ifstream file(filePath, std::ios::binary | std::ios::ate);
        if (!file) [[unlikely]] {
            return std::unexpected(VMError::InvalidOperand{"Failed to open file"});
        }
        std::streamsize size = file.tellg();
        file.seekg(0, std::ios::beg);
        std::vector<std::byte> buffer(size);
        if (!file.read(reinterpret_cast<char*>(buffer.data()), size)) [[unlikely]] {
            return std::unexpected(VMError::InvalidOperand{"Failed to read file"});
        }
        return buffer;
    }

    auto BytecodeLoader::validateHeader(const std::vector<std::byte>& buffer)
        -> std::expected<void, VMError> {
        if (buffer.size()< HEADER_SIZE) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{"Buffer too small for header"});

        if (!std::equal(buffer.begin(), buffer.begin()+ 4, MAGIC_NUMBER.begin())) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{"Invalid magic number"});

        uint8_t verMajor = static_cast<uint8_t>(buffer[4]);
        uint8_t verMinor = static_cast<uint8_t>(buffer[5]);
        if (verMajor != VERSION_MAJOR || verMinor != VERSION_MINOR) [[unlikely]] {
            return std::unexpected(VMError::InvalidOperand{"Unsupported version"});
        }

        return {};
    }

    // Helper functions (can be moved to a utility if shared)
    auto BytecodeLoader::decodeVarInt(std::vector<std::byte>::iterator& it, std::vector<std::byte>::iterator end)
        -> std::expected<int64_t, VMError> {
        int64_t result = 0;
        int shift = 0;
        while (it != end) [[likely]] {
            uint8_t byte = static_cast<uint8_t>(*it);
            ++it;
            result |= (byte & 0x7F) << shift;
            if (!(byte & 0x80)) [[likely]]
                break;
            shift += 7;
            if (shift >= 64) [[unlikely]]
                return std::unexpected(VMError::InvalidOperand{"VarInt too long"});
        }
        if (it == end && shift > 0) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{"Incomplete varint"});
        return result;
    }

    auto BytecodeLoader::decodeString(std::vector<std::byte>::iterator &it,
                                      std::vector<std::byte>::iterator end)
        -> std::expected<std::string, VMError> {
        auto lenRes = decodeVarInt(it, end);
        if (!lenRes) [[unlikely]]
            return std::unexpected(lenRes.error());
        size_t len = *lenRes;
        if (std::distance(it, end) < static_cast<ptrdiff_t>(len)) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{"Incomplete string"});
        std::string str(reinterpret_cast<const char*>(&*it), len);
        std::advance(it, len);
        return str;
    }
}
