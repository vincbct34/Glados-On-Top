/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Process Implementation
*/

#include "process/Process.hpp"

namespace rat
{

    /**
     * @brief Construct a new Process object
     * @param pid Process identifier
     * @param name Process name
     * @param tmpl Process template containing bytecode
     */
    Process::Process(ProcessId pid,
                     std::string name,
                     const ProcessTemplate &tmpl)
                    : _pid(pid)
                    , _name(std::move(name))
                    , _state(RuntimeValue::Type::Unit)
                    , _pc(0)
                    , _bytecode(tmpl.body())
    {
    }

    /**
     * @brief Get the process ID
     * @return ProcessId The unique process identifier
     */
    ProcessId Process::pid(void) const noexcept
    {
        return this->_pid;
    }

    /**
     * @brief Get the process name
     * @return const std::string& Reference to process name
     */
    const std::string &Process::name(void) const noexcept
    {
        return this->_name;
    }

    /**
     * @brief Get the current program counter
     * @return ProgramCounter Current instruction index
     */
    ProgramCounter Process::pc(void) const noexcept
    {
        return this->_pc;
    }

    /**
     * @brief Get the process state
     * @return const RuntimeValue& Reference to current state
     */
    const RuntimeValue &Process::state(void) const noexcept
    {
        return this->_state;
    }

    /**
     * @brief Get local variables store
     * @return VariableStore& Reference to local variables
     */
    VariableStore &Process::locals(void) noexcept
    {
        return this->_locals;
    }

    /**
     * @brief Get local variables store (const version)
     * @return const VariableStore& Const reference to local variables
     */
    const VariableStore &Process::locals(void) const noexcept
    {
        return this->_locals;
    }

    /**
     * @brief Get process mailbox
     * @return ProcessMailbox& Reference to message mailbox
     */
    ProcessMailbox &Process::mailbox(void) noexcept
    {
        return this->_mailbox;
    }

    /**
     * @brief Get process bytecode
     * @return const std::vector<Instruction>& Reference to bytecode instructions
     */
    const std::vector<Instruction> &Process::bytecode(void) const noexcept
    {
        return this->_bytecode;
    }

    /**
     * @brief Set the program counter
     * @param pc New program counter value
     */
    void Process::setPC(ProgramCounter pc) noexcept
    {
        this->_pc = pc;
    }

    /**
     * @brief Set the process state
     * @param state New state value
     */
    void Process::setState(RuntimeValue state)
    {
        this->_state = std::move(state);
    }

    /**
     * @brief Increment the program counter by one
     */
    void Process::incrementPC(void) noexcept
    {
        ++this->_pc;
    }

} 
