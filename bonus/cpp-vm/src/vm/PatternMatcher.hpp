/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** PatternMatcher - Handle pattern matching operations
*/

#pragma once

#include "core/RuntimeValue.hpp"
#include "vm/VariableStore.hpp"

#include <expected>
#include <string>

namespace rat
{
    /**
     * @class PatternMatcher
     * @brief Handles pattern matching operations for the VM.
     * Provides methods for matching atoms, tuples, literals, and wildcards.
     * @author Robin Toillon
     */
    class alignas(64) PatternMatcher {
        public:
            PatternMatcher() = default;
            ~PatternMatcher() = default;

            /**
             * @brief Match an atom pattern against a value
             * @param value The value to match
             * @param expectedAtom The expected atom name
             * @return true if matches, false otherwise
             */
            [[nodiscard]]
            bool matchAtom(const RuntimeValue &value,
                           const std::string &expectedAtom) const;

            /**
             * @brief Match a tuple pattern and extract elements
             * @param value The value to match
             * @param expectedSize The expected tuple size
             * @param elements Output vector for extracted elements
             * @return true if matches, false otherwise
             */
            [[nodiscard]]
            bool matchTuple(const RuntimeValue &value, size_t expectedSize,
                            std::vector<RuntimeValue> &elements) const;

            /**
             * @brief Match an integer literal
             * @param value The value to match
             * @param expected The expected integer value
             * @return true if matches, false otherwise
             */
            [[nodiscard]]
            bool matchInt(const RuntimeValue &value, int64_t expected) const;

            /**
             * @brief Match a boolean literal
             * @param value The value to match
             * @param expected The expected boolean value
             * @return true if matches, false otherwise
             */
            [[nodiscard]]
            bool matchBool(const RuntimeValue &value, bool expected) const;

            /**
             * @brief Match a string literal
             * @param value The value to match
             * @param expected The expected string value
             * @return true if matches, false otherwise
             */
            [[nodiscard]]
            bool matchString(const RuntimeValue &value,
                             const std::string &expected) const;

            /**
             * @brief Wildcard pattern - always matches
             * @return true always
             */
            [[nodiscard]]
            bool matchWildcard(void) const noexcept;

            /**
             * @brief Bind a value to a variable name
             * @param varName The variable name
             * @param value The value to bind
             * @param locals The local variable store
             * @return Expected void or error message
             */
            [[nodiscard]]
            auto bindVariable(const std::string &varName,
                              RuntimeValue value,
                              VariableStore &locals) const
                -> std::expected<void, VMError>;
    };
} 
