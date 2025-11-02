/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Arithmetic Instruction Handlers
*/

#pragma once

#include <expected>
#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

namespace rat::handlers
{
    /**
     * @brief Add two values from stack
     * @param instr Add instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleAdd(const Add& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Subtract two values from stack
     * @param instr Sub instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleSub(const Sub& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Multiply two values from stack
     * @param instr Mul instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleMul(const Mul& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Divide two values from stack
     * @param instr Div instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleDiv(const Div& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Negate a value from stack
     * @param instr Negate instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleNegate(const Negate& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Concatenate two strings from stack
     * @param instr Concat instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleConcat(const Concat& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Pre-increment a variable
     * @param instr IncVar instruction with variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleIncVar(const IncVar& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Pre-decrement a variable
     * @param instr DecVar instruction with variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleDecVar(const DecVar& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Post-increment a variable (push old value, then increment)
     * @param instr IncVarPost instruction with variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleIncVarPost(const IncVarPost& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Post-decrement a variable (push old value, then decrement)
     * @param instr DecVarPost instruction with variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleDecVarPost(const DecVarPost& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;

}
