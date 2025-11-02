/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Virtual Machine Class Definition
*/

#pragma once

#include <cstddef>
#include <expected>
#include <filesystem>
#include <string>
#include <variant>
#include <vector>
#include <unordered_map>
#include <memory>
#include <bitset>
#include "vm/Instruction.hpp"
#include "ExecutionEngine.hpp"
#include "InstructionDispatcher.hpp"
#include "VMContext.hpp"
#include "core/InstructionStack.hpp"
#include "core/RuntimeValue.hpp"
#include "loader/BytecodeLoader.hpp"
#include "loader/InstructionDecoder.hpp"
#include "vm/VariableStore.hpp"
#include "vm/FunctionManager.hpp"
#include "vm/ProcessManager.hpp"
#include "common/ErrorCode.hpp"

namespace rat
{
    /**
     * @class VirtualMachine
     * @brief Represents the GLaDOS Ratatouille Virtual Machine.
     * Composes decoupled components for loading, decoding, storing, and executing.
     * Uses modern C++23 features for performance and maintainability.
     * Uses the new optimized architecture with VMContext and InstructionDispatcher.
     * @author Robin Toillon
     */
    class VirtualMachine {
        public:
            VirtualMachine(void);
            ~VirtualMachine(void);

            [[nodiscard]]
            auto loadProgram(const std::filesystem::path &filePath)
                -> std::expected<void, VMError>;

            [[nodiscard]]
            auto run(void)
                -> std::expected<void, VMError>;

        private:
            // Core execution state
            Bytecode _bytecode;
            InstructionStack _stack;
            VariableStore _locals;
            VariableStore _globals;
            FunctionManager _functionManager;
            ProcessManager _processManager;
            RuntimeValue _processState;
            size_t _programCounter{0};
            std::bitset<8> _flags{};
            
            // Loading components
            BytecodeLoader _loader;
            InstructionDecoder _decoder;
            
            // Execution components (order matters - context must be initialized before dispatcher/engine)
            VMContext _context;
            InstructionDispatcher _dispatcher;
            ExecutionEngine _engine;
    };
}
