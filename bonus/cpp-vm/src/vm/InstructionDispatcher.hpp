/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Optimized Instruction Dispatcher - Direct function pointer table
*/

#pragma once

#include <array>
#include <expected>
#include <variant>

#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

namespace rat
{

    /**
     * @class InstructionDispatcher
     * @brief High-performance instruction dispatch using function pointer table
     *
     * Replaces std::function-based approach with direct function pointers.
     *
     * Benefits:
     * - No type erasure overhead
     * - No heap allocations
     * - Better branch prediction
     * - Smaller memory footprint (~544 bytes vs ~3KB)
     * - Cache-friendly layout
     *
     * Follows Open/Closed Principle - extend by adding new instruction types.
     *
     * @author Robin Toillon
     */
    class InstructionDispatcher {
        public:
            /**
             * @typedef HandlerFn
             * @brief Function pointer type for instruction handlers
             */
            using HandlerFn = std::expected<void, VMError> (*)(VMContext &);

            /**
             * @typedef TypedHandlerFn
             * @brief Template handler type for specific instruction types
             * @tparam InstrType The instruction type this handler processes
             */
            template<typename InstrType>
            using TypedHandlerFn
                = std::expected<void, VMError>(*)(const InstrType &,
                                                  VMContext &);

            /**
             * @brief Construct a new Instruction Dispatcher
             */
            InstructionDispatcher(void);

            /**
             * @brief Destroy the Instruction Dispatcher
             */
            ~InstructionDispatcher() = default;

            /**
             * @brief Dispatch an instruction to its registered handler
             * @param instr Instruction to execute
             * @param ctx VM execution context
             * @return std::expected<void, VMError> Success or execution error
             */
            [[nodiscard]] auto dispatch(
                const Instruction &instr,
                VMContext &ctx) -> std::expected<void, VMError>;

            /**
             * @brief Register a typed handler for a specific instruction type
             * @tparam InstrType The instruction type to handle
             * @param handler Function pointer to the handler
             */
            template<typename InstrType>
            void registerHandler(TypedHandlerFn<InstrType> handler);

        private:
            /**
             * @struct DispatchVisitor
             * @brief Optimized visitor that dispatches to function pointers
             */
            struct DispatchVisitor {
                VMContext &ctx;
                InstructionDispatcher &dispatcher;

                /**
                 * @brief Call operator for variant visitation
                 * @tparam InstrType The instruction type being visited
                 * @param instr The instruction instance
                 * @return std::expected<void, VMError> Execution result
                 */
                template<typename InstrType>
                auto operator()(const InstrType &instr)
                    -> std::expected<void, VMError>
                {
                    auto handler = dispatcher.getHandler<InstrType>();
                    if (handler) [[likely]]
                        return (*handler)(instr, ctx);
                    return std::unexpected(
                        VMError { VMError::InvalidState { "No handler registered" } });
                }
            };

            /**
             * @brief Get handler for specific instruction type
             * @tparam InstrType The instruction type
             * @return TypedHandlerFn<InstrType> Handler function pointer
             */
            template<typename InstrType>
            [[nodiscard]] TypedHandlerFn<InstrType> getHandler(void) const;

            /**
             * Storage for function pointers (one per instruction type)
             * Using variant index as lookup key - initialized to nullptr
             */
            std::array<void *, 68> _handlers {};

            friend struct DispatchVisitor;
    };

}

#include "InstructionDispatcher.inl"
