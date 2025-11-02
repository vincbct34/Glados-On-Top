/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** IFunctionManager - Interface for function management
*/

#pragma once

#include <expected>
#include <string>
#include <vector>
#include <optional>
#include "core/RuntimeValue.hpp"

namespace rat
{
    struct Instruction;
}

namespace rat::interfaces
{
    /**
     * @interface IFunctionManager
     * @brief Interface for managing function definitions and call stack
     */
    class IFunctionManager {
    public:
        virtual ~IFunctionManager(void)= default;

        /**
         * @brief Define a function
         */
        virtual std::expected<void, std::string> defineFunction(
            const std::string& name,
            const std::vector<std::string>& parameters,
            const InstructionVector& body) = 0;

        /**
         * @brief Get a function
         */
        virtual std::optional<const InstructionVector*> getFunction(const std::string& name) const = 0;

        /**
         * @brief Push call frame
         */
        virtual std::expected<void, std::string> pushCallFrame(
            const std::string& functionName,
            const std::vector<RuntimeValue>& args) = 0;

        /**
         * @brief Pop call frame
         */
        virtual std::expected<std::string, std::string> popCallFrame(void)= 0;
    };

} ::interfaces
