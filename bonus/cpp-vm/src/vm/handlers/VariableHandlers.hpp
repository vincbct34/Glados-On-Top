/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Variable and Storage Instruction Handlers
*/

#pragma once

#include <expected>
#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

namespace rat::handlers
{
    /**
     * @brief Load a global variable value onto the stack
     * @param instr Load instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleLoadVar(const LoadVar& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Store a value from stack into a global variable
     * @param instr Store instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleStoreVar(const StoreVar& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Load a local variable value onto the stack
     * @param instr Load instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleLoadLocal(const LoadLocal& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Store a value from stack into a local variable
     * @param instr Store instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleStoreLocal(const StoreLocal& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Initialize process state from stack value
     * @param instr Init instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleInitState(const InitState& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push current process state onto the stack
     * @param instr Get instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleGetState(const GetState& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Set process state from stack value
     * @param instr Set instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleSetState(const SetState& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Index into an array and push element onto stack
     * @param instr Index instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleIndex(const Index& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Get the length of an array
     * @param instr ArrayLength instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleArrayLength(const ArrayLength& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Extract a field from a tuple or struct
     * @param instr GetField instruction with field name/index
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleGetField(const GetField& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;

}
