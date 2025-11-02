/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Comparison and Logic Instruction Handlers
*/

#pragma once

#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

#include <expected>

namespace rat::handlers
{
    /**
     * @brief Compare two values for equality
     * @param instr CmpEq instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleCmpEq(const CmpEq &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Compare two values for inequality
     * @param instr CmpNeq instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleCmpNeq(const CmpNeq &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Compare if left < right
     * @param instr CmpLt instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleCmpLt(const CmpLt &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Compare if left <= right
     * @param instr CmpLe instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleCmpLe(const CmpLe &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Compare if left > right
     * @param instr CmpGt instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleCmpGt(const CmpGt &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Compare if left >= right
     * @param instr CmpGe instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleCmpGe(const CmpGe &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Logical AND of two boolean values
     * @param instr LogicAnd instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleLogicAnd(const LogicAnd &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Logical OR of two boolean values
     * @param instr LogicOr instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleLogicOr(const LogicOr &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Logical NOT of a boolean value
     * @param instr LogicNot instruction
     * @param ctx VM execution context
     * @return Success or VMError
     */
    [[nodiscard]] auto handleLogicNot(const LogicNot &instr, VMContext &ctx)
        -> std::expected<void, VMError>;

}
