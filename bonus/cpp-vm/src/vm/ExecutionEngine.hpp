/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Optimized Execution Engine - Header Only
*/

#pragma once

#include "common/ErrorCode.hpp"
#include "vm/InstructionDispatcher.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

#include <bitset>
#include <cstddef>
#include <expected>

namespace rat {
    /**
     * @class ExecutionEngine
     * @brief Optimized execution engine using direct dispatch.
     *
     * SOLID Principles Applied:
     * - Single Responsibility: Only manages execution loop
     * - Open/Closed: Extend via new instruction handlers
     * - Dependency Inversion: Depends on abstractions (VMContext, Dispatcher)
     *
     * Performance Optimizations:
     * - Cache-aligned data layout
     * - Hot/cold data separation
     * - Direct function pointer dispatch
     * - Branch prediction hints
     * - Minimal indirection
     */
    class alignas(64) ExecutionEngine {
        public:
            /**
             * @brief Construct execution engine with context
             * @param bytecode The bytecode to execute
             * @param ctx The execution context (contains stack, vars, etc.)
             * @param dispatcher The instruction dispatcher
             */
            ExecutionEngine(const Bytecode &bytecode,
                VMContext &ctx,
                InstructionDispatcher &dispatcher);

            ~ExecutionEngine(void) = default;

            // Non-copyable, movable
            ExecutionEngine(const ExecutionEngine &) = delete;
            ExecutionEngine &operator=(const ExecutionEngine &) = delete;
            ExecutionEngine(ExecutionEngine &&) noexcept = default;
            ExecutionEngine &operator=(ExecutionEngine &&) noexcept = default;

            /**
             * @brief Execute bytecode until halt or error
             * @return Success or error
             *
             * Hot loop optimized for:
             * - Cache locality (sequential access)
             * - Branch prediction (likely/unlikely hints)
             * - Minimal overhead per instruction
             */
            [[nodiscard]]
            auto run(void) -> std::expected<void, VMError>;

            /**
             * @brief Execute single instruction
             * @return Success or error
             * Used for debugging and step execution
             */
            [[nodiscard]]
            auto step(void) -> std::expected<void, VMError>;

            /**
             * @brief Check if engine is still running
             */
            [[nodiscard]]
            bool isRunning() const noexcept;

            /**
             * @brief Reset execution state
             */
            void reset() noexcept;

        private:
            // Hot data - accessed every instruction (cache line 1)
            const Bytecode &_bytecode; // 8 bytes (reference)
            VMContext &_context; // 8 bytes (reference)
            InstructionDispatcher &_dispatcher; // 8 bytes (reference)
            // Total hot data: 24 bytes (fits in single cache line)

            // Padding to next cache line
            char _padding[40];
    };

}
