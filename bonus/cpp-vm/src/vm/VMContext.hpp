/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** VM Context - Execution state container (Header)
*/

#pragma once

#include <bitset>
#include <cstddef>

namespace rat
{
    // Forward declarations
    class InstructionStack;
    class VariableStore;
    class FunctionManager;
    class ProcessManager;
    class RuntimeValue;

    /**
     * @class VMContext
     * @brief Execution context for VM operations.
     *
     * SOLID Principles:
     * - Single Responsibility: Encapsulates only execution state
     * - Interface Segregation: Minimal public interface
     *
     * Performance Optimizations:
     * - Cache-aligned to 64 bytes (typical cache line size)
     * - Hot data (frequently accessed) placed first
     * - Warm data (often accessed) in middle
     * - Cold data (rarely accessed) at end
     * - References to avoid pointer indirection
     *
     * Memory Layout (optimized for cache locality):
     * [0-15]   Hot:  stack, programCounter, flags (16 bytes)
     * [16-31]  Warm: locals, globals (16 bytes)
     * [32-63]  Cold: functionManager, processManager, processState (32 bytes)
     */
    class alignas(64) VMContext {
    public:
        VMContext(InstructionStack &stk,
                size_t &pc,
                std::bitset<8> &flgs,
                VariableStore &loc,
                VariableStore &glob,
                FunctionManager &funcMgr,
                ProcessManager &procMgr,
                RuntimeValue &procState);

        ~VMContext() = default;

        VMContext(const VMContext &) = delete;
        VMContext &operator=(const VMContext &) = delete;
        VMContext(VMContext &&) noexcept = default;
        VMContext &operator=(VMContext &&) noexcept = default;

        [[nodiscard]]
        InstructionStack &stack(void) noexcept;

        [[nodiscard]]
        const InstructionStack &stack(void) const noexcept;

        [[nodiscard]]
        size_t &programCounter(void) noexcept;

        [[nodiscard]]
        size_t programCounter(void) const noexcept;

        [[nodiscard]]
        bool isRunning(void) const noexcept;

        void setRunning(bool running) noexcept;

        [[nodiscard]]
        VariableStore &locals(void) noexcept;

        [[nodiscard]]
        const VariableStore &locals(void) const noexcept;

        [[nodiscard]]
        VariableStore &globals(void) noexcept;

        [[nodiscard]]
        const VariableStore &globals(void) const noexcept;

        [[nodiscard]]
        FunctionManager &functionManager(void) noexcept;

        [[nodiscard]]
        const FunctionManager &functionManager(void) const noexcept;

        [[nodiscard]]
        ProcessManager &processManager(void) noexcept;

        [[nodiscard]]
        const ProcessManager &processManager(void) const noexcept;

        [[nodiscard]]
        RuntimeValue &processState(void) noexcept;
        [[nodiscard]]
        const RuntimeValue &processState(void) const noexcept;

    private:
        InstructionStack &_stack;
        size_t &_programCounter;

        std::bitset<8> &_flags;
        VariableStore &_locals;
        VariableStore &_globals;

        FunctionManager &_functionManager;
        ProcessManager &_processManager;
        RuntimeValue &_processState;
    };
} 
