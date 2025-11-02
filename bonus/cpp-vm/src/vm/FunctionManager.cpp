/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** FunctionManager Implementation
*/

#include "FunctionManager.hpp"
#include <format>

namespace rat
{
    auto FunctionManager::defineFunction(const std::string &name,
                                         const std::vector<std::string> &parameters,
                                         const std::vector<Instruction> &body)
        -> std::expected<void, VMError>
    {
        this->_functions[name] = FunctionDef(name, parameters, Bytecode{body});
        return {};
    }

    auto FunctionManager::getFunction(const std::string &name) const
        -> std::expected<const FunctionDef *, VMError>
    {
        auto it = this->_functions.find(name);
        if (it == this->_functions.end())
            return std::unexpected(VMError::FunctionNotFound{name});
        return &it->second;
    }

    auto FunctionManager::pushCallFrame(const std::string &functionName, size_t returnPC,
                                        const VariableStore &locals)
        -> std::expected<void, VMError>
    {
        if (this->_callStack.size()>= MAX_CALL_DEPTH)
            return std::unexpected(VMError::InvalidState{"Stack overflow: maximum call depth exceeded"});

        this->_callStack.emplace(functionName, returnPC, locals);
        return {};
    }

    auto FunctionManager::popCallFrame(void)-> std::expected<CallFrame, VMError>
    {
        if (this->_callStack.empty())
            return std::unexpected(VMError::InvalidState{"Call stack underflow"});

        CallFrame frame = std::move(_callStack.top());
        this->_callStack.pop();
        return frame;
    }

    auto FunctionManager::pushCallFrame(const std::string &functionName,
                                        [[maybe_unused]] const std::vector<RuntimeValue> &args)
        -> std::expected<void, VMError>
    {
        if (this->_callStack.size()>= MAX_CALL_DEPTH)
            return std::unexpected(VMError::InvalidState{"Stack overflow: maximum call depth exceeded"});

        VariableStore emptyLocals;
        this->_callStack.emplace(functionName, 0, std::move(emptyLocals));
        return {};
    }

    auto FunctionManager::getFunction(const std::string &name)
        -> std::optional<const FunctionDef *>
    {
        auto it = this->_functions.find(name);
        if (it == this->_functions.end())
            return std::nullopt;
        return &it->second;
    }
}
