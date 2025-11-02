/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** VariableStore Class Definition
*/

#pragma once

#include <unordered_map>
#include <string>
#include <expected>
#include "core/RuntimeValue.hpp"
#include "common/ErrorCode.hpp"

namespace rat
{
    /**
     * @class VariableStore
     * @brief Manages variable storage for locals and globals.
     * Uses std::unordered_map for O(1) average access, cache-aligned.
     * Provides functional-style operations.
     * @author Robin Toillon
     */
    class alignas(64) VariableStore {
        public:
            VariableStore() = default;
            ~VariableStore() = default;

            [[nodiscard]]
            auto get(const std::string &name) const
                -> std::expected<RuntimeValue, VMError>;

            [[nodiscard]]
            auto set(const std::string &name, RuntimeValue value)
                -> std::expected<void, VMError>;

            void clear(void)  noexcept;

            template <typename Func>
            void forEach(Func &&func) const;

        private:
            std::unordered_map<std::string, RuntimeValue> _store;
    };
}

#include "VariableStore.tpp"
