/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** IPatternMatcher - Interface for pattern matching operations
*/

#pragma once

#include <expected>
#include <string>
#include "core/RuntimeValue.hpp"

namespace rat::interfaces
{
    /**
     * @interface IPatternMatcher
     * @brief Interface for pattern matching operations in the VM
     */
    class IPatternMatcher {
    public:
        virtual ~IPatternMatcher(void)= default;

        /**
         * @brief Match an atom pattern
         */
        virtual bool matchAtom(const RuntimeValue& value, const std::string& expectedAtom) const = 0;

        /**
         * @brief Match a tuple pattern
         */
        virtual bool matchTuple(const RuntimeValue& value, size_t expectedSize,
                               std::vector<RuntimeValue>& elements) const = 0;

        /**
         * @brief Match an integer pattern
         */
        virtual bool matchInt(const RuntimeValue& value, int64_t expected) const = 0;

        /**
         * @brief Match a boolean pattern
         */
        virtual bool matchBool(const RuntimeValue& value, bool expected) const = 0;

        /**
         * @brief Match a string pattern
         */
        virtual bool matchString(const RuntimeValue& value, const std::string& expected) const = 0;

        /**
         * @brief Bind a variable
         */
        virtual std::expected<void, std::string> bindVariable(
            const std::string& varName, RuntimeValue value) = 0;
    };

} ::interfaces
