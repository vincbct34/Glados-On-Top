/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Optimized Execution Engine
*/

#pragma once

#include "common/ErrorCode.hpp"
#include "core/InstructionStack.hpp"
#include "vm/InstructionDispatcher.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

#include <bitset>
#include <cstddef>
#include <expected>

namespace rat
{
    /**
     * @class ExecutionEngine
     * @brief Optimized bytecode execution engine.
     *
     * Responsibilities (Single Responsibility Principle):
     * - Manage program counter and execution flow
     * - Coordinate instruction dispatch
     * - Handle execution state (running/halted)
     *
     * Optimizations:
     * - Cache-aligned for better performance
     * - Minimized data members (only execution state)
     * - Branch prediction hints
     * - Prefetching support (future)
     *
     * @author Robin Toillon
     */
    class alignas(64) ExecutionEngine {
        public:
            ExecutionEngine(InstructionStack &stack,
                const Bytecode &bytecode,
                VMContext &context,
                InstructionDispatcher &dispatcher);
            ~ExecutionEngine() = default;

            // Deleted copy/move to ensure single ownership of execution state
            ExecutionEngine(const ExecutionEngine &) = delete;
            ExecutionEngine &operator=(const ExecutionEngine &) = delete;
            ExecutionEngine(ExecutionEngine &&) = delete;
            ExecutionEngine &operator=(ExecutionEngine &&) = delete;

            /**
             * @brief Main execution loop - optimized for performance
             * @return void on success, VMError on failure
             */
            [[nodiscard]]
            auto run(void) -> std::expected<void, VMError>;

            // Accessors
            [[nodiscard]] size_t getProgramCounter(void) const noexcept
            {
                return _pc;
            }
            void setProgramCounter(size_t pc) noexcept
            {
                _pc = pc;
            }

            [[nodiscard]] bool isRunning() const noexcept
            {
                return _flags.test(0);
            }
            void setRunning(bool running) noexcept
            {
                _flags.set(0, running);
            }

        private:
            // Hot data - frequently accessed (cache line 0)
            size_t _pc; // Program counter (8 bytes)
            std::bitset<8> _flags; // Execution flags (1 byte + padding)
            const Bytecode &_bytecode; // Reference to bytecode (8 bytes)
            InstructionStack &_stack; // Reference to stack (8 bytes)

            // Warm data - moderately accessed (cache line 0/1)
            VMContext &_context; // Execution context (8 bytes)
            InstructionDispatcher &_dispatcher; // Instruction dispatcher (8 bytes)

            // Helper methods
            [[nodiscard]]
            auto fetchNextInstruction(void) -> const Instruction *;

            void advanceProgramCounter(void) noexcept
            {
                ++_pc;
            }
    };

}
