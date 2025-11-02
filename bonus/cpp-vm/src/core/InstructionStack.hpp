/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Instruction Stack Class Definition
*/

#pragma once

#include <vector>
#include <expected>
#include <ranges>
#include <algorithm>
#include <functional>
#include "RuntimeValue.hpp"
#include "common/ErrorCode.hpp"

namespace rat
{
    /**
     * @class InstructionStack
     * @brief Represents the operand stack of the GLaDOS Ratatouille Virtual Machine.
     * This class manages the stack of runtime values during bytecode execution.
     * @author Robin Toillon
     */
    class alignas(64) InstructionStack {
        public:
            InstructionStack(size_t maxSize = 1024);
            ~InstructionStack(void)= default;

            [[nodiscard]]
                auto push(const RuntimeValue &val)
                    -> std::expected<void, VMError>;

                [[nodiscard]]
                auto pop(void)
                    -> std::expected<RuntimeValue, VMError>;

                [[nodiscard]]
                auto peek(size_t offset = 0) const
                    -> std::expected<std::reference_wrapper<const RuntimeValue>, VMError>;

            [[nodiscard]]
            size_t size(void) const noexcept;
            void clear(void) noexcept;

            template<typename Func>
            void forEach(Func &&func) const;

        private:
            std::vector<RuntimeValue> _stack;
            size_t _maxSize;

    };
}

#include "InstructionStack.tpp"