/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Process - Entity representing a running process instance
*/

#pragma once

#include <string>

#include "common/Types.hpp"
#include "core/ProgramCounter.hpp"
#include "core/RuntimeValue.hpp"
#include "process/ProcessMailbox.hpp"
#include "process/ProcessTemplate.hpp"
#include "vm/Instruction.hpp"
#include "vm/VariableStore.hpp"

namespace rat
{

/**
 * @class Process
 * @brief Represents a running process instance
 *
 * Domain entity that encapsulates the state of a running process.
 * Contains execution state, local variables, mailbox, and bytecode.
 * Encapsulates process state with proper access control.
 *
 * @author Robin Toillon
 */
class Process {
public:
    /**
     * @brief Construct a process from a template
     * @param pid Unique process ID
     * @param name Process name
     * @param tmpl Template to instantiate from
     */
    Process(ProcessId pid, std::string name, const ProcessTemplate &tmpl);

    // Disable copy for entity semantics
    Process(const Process &) = delete;
    Process &operator=(const Process &) = delete;

    // Allow move
    Process(Process &&) noexcept = default;
    Process &operator=(Process &&) noexcept = default;

    // === Accessors ===

    /**
     * @brief Get process ID
     * @return ProcessId The unique process identifier
     */
    [[nodiscard]] ProcessId pid(void) const noexcept;

    /**
     * @brief Get process name
     * @return const std::string& Reference to process name
     */
    [[nodiscard]] const std::string &name(void) const noexcept;

    /**
     * @brief Get program counter
     * @return ProgramCounter Current instruction index
     */
    [[nodiscard]] ProgramCounter pc(void) const noexcept;

    /**
     * @brief Get process state
     * @return const RuntimeValue& Reference to current state value
     */
    [[nodiscard]] const RuntimeValue &state(void) const noexcept;

    /**
     * @brief Get local variables store
     * @return VariableStore& Reference to local variables
     */
    [[nodiscard]] VariableStore &locals(void) noexcept;

    /**
     * @brief Get local variables store (const version)
     * @return const VariableStore& Const reference to local variables
     */
    [[nodiscard]] const VariableStore &locals(void) const noexcept;

    /**
     * @brief Get process mailbox
     * @return ProcessMailbox& Reference to message mailbox
     */
    [[nodiscard]] ProcessMailbox &mailbox(void) noexcept;

    /**
     * @brief Get bytecode instructions
     * @return const std::vector<Instruction>& Reference to bytecode sequence
     */
    [[nodiscard]] const std::vector<Instruction> &bytecode(void) const noexcept;

    /**
     * @brief Set program counter
     * @param pc New PC value
     */
    void setPC(ProgramCounter pc) noexcept;

    /**
     * @brief Set process state
     * @param state New state value
     */
    void setState(RuntimeValue state);

    /**
     * @brief Increment program counter by one
     */
    void incrementPC(void) noexcept;

private:
    ProcessId _pid;
    std::string _name;
    VariableStore _locals;
    RuntimeValue _state;
    ProcessMailbox _mailbox;
    ProgramCounter _pc;
    std::vector<Instruction> _bytecode;
};

} 
