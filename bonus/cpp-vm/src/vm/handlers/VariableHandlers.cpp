/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Variable and Storage Instruction Handlers Implementation
*/

#include "VariableHandlers.hpp"

#include "core/InstructionStack.hpp"
#include "core/RuntimeValue.hpp"
#include "vm/VariableStore.hpp"

namespace rat::handlers
{
    /**
     * @brief Load a global variable value onto the stack
     * @param instr Load instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError if variable not found or stack push fails
     */
    auto handleLoadVar(const LoadVar &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        auto val = ctx.globals().get(instr.name);
        if (!val) [[unlikely]]
            return std::unexpected(VMError::VariableNotFound {instr.name});

        auto result = ctx.stack().push(*val);
        if (!result) [[unlikely]] 
            return std::unexpected(result.error());
        return {};
    }

    /**
     * @brief Store a value from stack into a global variable
     * @param instr Store instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError if stack underflow or store fails
     */
    auto handleStoreVar(const StoreVar &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]] {
            return std::unexpected(VMError::StackUnderflow{});
        }

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        auto result = ctx.globals().set(instr.name, std::move(*val));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    /**
     * @brief Load a local variable value onto the stack
     * @param instr Load instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError if variable not found or stack push fails
     */
    auto handleLoadLocal(const LoadLocal &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        auto val = ctx.locals().get(instr.name);
        if (!val) [[unlikely]]
            return std::unexpected(VMError::VariableNotFound{instr.name});

        auto result = ctx.stack().push(*val);
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    /**
     * @brief Store a value from stack into a local variable
     * @param instr Store instruction containing variable name
     * @param ctx VM execution context
     * @return Success or VMError if stack underflow or store fails
     */
    auto handleStoreLocal(const StoreLocal &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]] {
            return std::unexpected(val.error());
        }

        auto result = ctx.locals().set(instr.name, std::move(*val));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    /**
     * @brief Initialize process state from stack value
     * @param instr Init instruction (unused)
     * @param ctx VM execution context
     * @return Success or VMError if stack underflow
     */
    auto handleInitState([[maybe_unused]] const InitState &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        ctx.processState() = std::move(*val);
        return {};
    }

    /**
     * @brief Push current process state onto the stack
     * @param instr Get instruction (unused)
     * @param ctx VM execution context
     * @return Success or VMError if stack push fails
     */
    auto handleGetState([[maybe_unused]] const GetState &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(ctx.processState());
        if (!result) [[unlikely]]
            return std::unexpected(result.error());

        return {};
    }

    /**
     * @brief Set process state from stack value
     * @param instr Set instruction (unused)
     * @param ctx VM execution context
     * @return Success or VMError if stack underflow
     */
    auto handleSetState([[maybe_unused]] const SetState &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        ctx.processState() = std::move(*val);
        return {};
    }

    /**
     * @brief Index into an array and push element onto stack
     * @param instr Index instruction (unused)
     * @param ctx VM execution context
     * @return Success or VMError if invalid operands or out of bounds
     */
    auto handleIndex(const Index &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() < 2) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto index = ctx.stack().pop();
        auto array = ctx.stack().pop();

        if (!index || !array) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        if (index->type() != RuntimeValue::Type::I32) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Index must be an integer"});

        if (array->type() != RuntimeValue::Type::Array) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Indexing requires an array"});

        auto arr = array->getIf<std::vector<RuntimeValue>>();

        if (!arr) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Failed to get array"});

        auto idxVal = index->getIf<std::int32_t>();
        auto idx = static_cast<size_t>(idxVal->get());

        if (idx >= arr->get().size()) [[unlikely]]
            return std::unexpected(VMError::OutOfBounds{});

        auto result = ctx.stack().push(arr->get()[idx]);

        if (!result) [[unlikely]]
            return std::unexpected(result.error());

        return {};
    }

    /**
     * @brief Get the length of an array and push it onto the stack
     * @param instr ArrayLength instruction (unused)
     * @param ctx VM execution context
     * @return Success or VMError if invalid operand
     */
    auto handleArrayLength(const ArrayLength &, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto array = ctx.stack().pop();

        if (!array) [[unlikely]]
            return std::unexpected(array.error());

        if (array->type() != RuntimeValue::Type::Array) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "ArrayLength requires an array"});

        auto arr = array->getIf<std::vector<RuntimeValue>>();

        if (!arr) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Failed to get array"});

        auto length = static_cast<std::int64_t>(arr->get().size());
        auto result = ctx.stack().push(RuntimeValue(length));

        if (!result) [[unlikely]]
            return std::unexpected(result.error());

        return {};
    }

    /**
     * @brief Extract a field from a tuple or struct
     * @param instr GetField instruction with field name/index
     * @param ctx VM execution context
     * @return Success or VMError if invalid operand or out of bounds
     */
    auto handleGetField(const GetField &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();

        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        const auto &fieldName = instr.field;

        if (val->type() == RuntimeValue::Type::Tuple) {
            auto tuple = val->getIf<std::vector<RuntimeValue>>();

            if (!tuple) [[unlikely]]
                return std::unexpected(VMError::InvalidOperand{
                    "Failed to get tuple"});

            try {
                size_t index = std::stoull(fieldName);

                if (index >= tuple->get().size()) [[unlikely]]
                    return std::unexpected(VMError::InvalidOperand{
                        "Tuple index out of bounds: " + std::to_string(index)});

                auto result = ctx.stack().push(tuple->get()[index]);

                if (!result) [[unlikely]]
                    return std::unexpected(result.error());

                return {};
            } catch (...) {
                return std::unexpected(VMError::InvalidOperand{
                    "Invalid tuple field name (must be numeric): " 
                    + fieldName});
            }
        }

        return std::unexpected(VMError::InvalidState{
            "GetField not supported for this type"});
    }

}
