/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine  
** File description:
** VM Context - Implementation
*/

#include "VMContext.hpp"
#include "core/InstructionStack.hpp"
#include "vm/VariableStore.hpp"
#include "vm/FunctionManager.hpp"
#include "vm/ProcessManager.hpp"
#include "core/RuntimeValue.hpp"

namespace rat
{
    VMContext::VMContext(InstructionStack &stk, size_t &pc,
                         std::bitset<8> &flgs,
                         VariableStore &loc, VariableStore &glob,
                         FunctionManager &funcMgr, ProcessManager &procMgr,
                         RuntimeValue &procState)
                        : _stack(stk)
                        , _programCounter(pc)
                        , _flags(flgs)
                        , _locals(loc)
                        , _globals(glob)
                        , _functionManager(funcMgr)
                        , _processManager(procMgr)
                        , _processState(procState)
    {
    }

    bool VMContext::isRunning(void) const noexcept
    {
        return this->_flags.test(0);
    }

    void VMContext::setRunning(bool running) noexcept
    {
        this->_flags.set(0, running);
    }

    InstructionStack &VMContext::stack(void) noexcept
    {
        return this->_stack;
    }

    const InstructionStack &VMContext::stack(void) const noexcept
    {
        return this->_stack;
    }

    size_t &VMContext::programCounter(void) noexcept
    {
        return this->_programCounter;
    }

    size_t VMContext::programCounter(void) const noexcept
    {
        return this->_programCounter;
    }

    VariableStore &VMContext::locals(void) noexcept
    {
        return this->_locals;
    }
    const VariableStore &VMContext::locals(void) const noexcept
    {
        return this->_locals;
    }

    VariableStore &VMContext::globals(void) noexcept
    {
        return this->_globals;
    }
    const VariableStore &VMContext::globals(void) const noexcept
    {
        return this->_globals;
    }

    FunctionManager &VMContext::functionManager(void) noexcept
    {
        return this->_functionManager;
    }
    const FunctionManager &VMContext::functionManager(void) const noexcept
    {
        return this->_functionManager;
    }

    ProcessManager &VMContext::processManager(void) noexcept
    {
        return this->_processManager;
    }
    const ProcessManager &VMContext::processManager(void) const noexcept
    {
        return this->_processManager;
    }

    RuntimeValue &VMContext::processState(void) noexcept
    {
        return this->_processState;
    }

    const RuntimeValue &VMContext::processState(void) const noexcept
    {
        return this->_processState;
    }
}
