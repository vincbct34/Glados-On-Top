/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** VirtualMachine Class Implementation - Updated for new architecture
*/

#include "VirtualMachine.hpp"
#include <algorithm>
#include <expected>
#include <filesystem>
#include <iostream>

namespace rat
{
    VirtualMachine::VirtualMachine(void) : _processState(RuntimeValue::Type::Unit)
                                         , _context(_stack,
                                                    _programCounter,
                                                    _flags,
                                                    _locals, _globals, 
                                                    _functionManager,
                                                    _processManager,
                                                    _processState)
                                         , _dispatcher()
                                         , _engine(_bytecode, _context,
                                                   _dispatcher)
    {
    }

    VirtualMachine::~VirtualMachine() = default;

    auto VirtualMachine::loadProgram(const std::filesystem::path &filePath)
        -> std::expected<void, VMError>
    {
        auto bufferRes = this->_loader.loadFromFile(filePath);
        if (!bufferRes.has_value())
            return std::unexpected(bufferRes.error());

        auto decodeRes = this->_decoder.decode(*bufferRes);
        if (!decodeRes.has_value())
            return std::unexpected(decodeRes.error());

        this->_bytecode.instructions = std::move(*decodeRes);
        return {};
    }

    auto VirtualMachine::run(void) -> std::expected<void, VMError>
    {
        return this->_engine.run();
    }

}
