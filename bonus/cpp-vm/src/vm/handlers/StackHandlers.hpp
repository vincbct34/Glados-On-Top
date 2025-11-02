/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Stack Instruction Handlers - Push/Pop operations
*/

#pragma once

#include <expected>
#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

namespace rat::handlers
{
    /**
     * @brief Push an integer value onto the stack
     * @param instr PushInt instruction with value
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushInt(const PushInt& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push a float value onto the stack
     * @param instr PushFloat instruction with value
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushFloat(const PushFloat& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push a string value onto the stack
     * @param instr PushString instruction with value
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushString(const PushString& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push an atom value onto the stack
     * @param instr PushAtom instruction with value
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushAtom(const PushAtom& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push a tuple onto the stack
     * @param instr PushTuple instruction with size
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushTuple(const PushTuple& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push an array onto the stack
     * @param instr PushArray instruction with size
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushArray(const PushArray& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push a unit value onto the stack
     * @param instr PushUnit instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushUnit(const PushUnit& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push a boolean value onto the stack
     * @param instr PushBool instruction with value
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushBool(const PushBool& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Push a None value onto the stack
     * @param instr PushNone instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePushNone(const PushNone& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Pop N values from the stack
     * @param instr PopN instruction with count
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handlePopN(const PopN& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;
    
    /**
     * @brief Duplicate the top value on the stack
     * @param instr Dup instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]]
    auto handleDup(const Dup& instr, VMContext& ctx) 
        -> std::expected<void, VMError>;

}
