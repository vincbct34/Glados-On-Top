/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** FunctionManager - Manage function definitions and call stack
*/

#pragma once

#include "core/RuntimeValue.hpp"
#include "vm/VariableStore.hpp"
#include "vm/Instruction.hpp"

#include <expected>
#include <map>
#include <optional>
#include <stack>
#include <string>
#include <vector>

namespace rat
{
    /**
     * @struct CallFrame
     * @brief Represents a function call frame on the call stack
     */
    struct CallFrame {
        std::string functionName;
        size_t returnPC;
        VariableStore savedLocals;

        CallFrame(std::string name, size_t pc, VariableStore locals)
            : functionName(std::move(name))
            , returnPC(pc)
            , savedLocals(std::move(locals))
        {
        }
    };

    /**
     * @class FunctionManager
     * @brief Manages function definitions and the call stack
     * @author Robin Toillon
     */
    class alignas(64) FunctionManager {
        public:
            FunctionManager() = default;
            ~FunctionManager() = default;

            /**
             * @brief Define a new function
             * @param name Function name
             * @param parameters Parameter names
             * @param body Function bytecode
             * @return Expected void or error
             */
            [[nodiscard]]
            auto defineFunction(const std::string &name,
                                const std::vector<std::string> &parameters,
                                const std::vector<Instruction> &body)
                -> std::expected<void, VMError>;

            /**
             * @brief Get a function definition
             * @param name Function name
             * @return Expected function definition or error
             */
            [[nodiscard]]
            auto getFunction(const std::string &name) const
                -> std::expected<const FunctionDef *, VMError>;

            /**
             * @brief Push a call frame onto the call stack
             * @param functionName Name of the function being called
             * @param returnPC PC to return to after function
             * @param locals Current local variables
             * @return Expected void or error if stack overflow
             */
            [[nodiscard]]
            auto pushCallFrame(const std::string &functionName,
                               size_t returnPC,
                               const VariableStore &locals)
                -> std::expected<void, VMError>;

            /**
             * @brief Pop a call frame from the call stack
             * @return Expected call frame or error if stack is empty
             */
            [[nodiscard]]
            auto popCallFrame(void) -> std::expected<CallFrame, VMError>;

            /**
             * @brief Push a call frame with arguments
             * @param functionName Name of the function being called
             * @param args Function arguments
             * @return Expected void or error
             */
            [[nodiscard]]
            auto pushCallFrame(const std::string &functionName,
                               const std::vector<RuntimeValue> &args)
                -> std::expected<void, VMError>;

            /**
             * @brief Get function definition (optional return)
             * @param name Function name
             * @return Optional function definition
             */
            [[nodiscard]]
            auto getFunction(const std::string &name)
                -> std::optional<const FunctionDef *>;

        private:
            std::map<std::string, FunctionDef> _functions;
            std::stack<CallFrame> _callStack;
            static constexpr size_t MAX_CALL_DEPTH = 1000;
    };
} 
