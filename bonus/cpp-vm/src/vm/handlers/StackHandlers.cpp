/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Stack Instruction Handlers Implementation
*/

#include "StackHandlers.hpp"
#include "core/InstructionStack.hpp"
#include "core/RuntimeValue.hpp"

namespace rat::handlers
{
    auto handlePushInt(const PushInt& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(
            RuntimeValue(static_cast<std::int64_t>(instr.value)));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePushFloat(const PushFloat& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(RuntimeValue(instr.value));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePushString(const PushString& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(RuntimeValue(instr.value,
                                                    RuntimeValue::Type::String));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePushAtom(const PushAtom& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(RuntimeValue(instr.value,
                                                    RuntimeValue::Type::Atom));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePushTuple(const PushTuple& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        if (instr.size < 0) [[unlikely]]
            return std::unexpected(
                VMError::InvalidState{"Tuple size must be non-negative"});
        
        const auto size = static_cast<size_t>(instr.size);
        if (ctx.stack().size()< size) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        std::vector<RuntimeValue> elements;
        elements.reserve(size);

        for (size_t i = 0; i < size; ++i) {
            auto elem = ctx.stack().pop();
            if (!elem) [[unlikely]]
                return std::unexpected(elem.error());
            elements.insert(elements.begin(), std::move(*elem));
        }

        auto result = ctx.stack().push(
            RuntimeValue(elements, RuntimeValue::Type::Tuple));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePushArray(const PushArray& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        if (instr.size < 0) [[unlikely]]
            return std::unexpected(
                VMError::InvalidState{"Array size must be non-negative"});
        
        const auto size = static_cast<size_t>(instr.size);
        if (ctx.stack().size()< size) [[unlikely]] {
            return std::unexpected(VMError::StackUnderflow{});
        }

        std::vector<RuntimeValue> elements;
        elements.reserve(size);
        
        for (size_t i = 0; i < size; ++i) {
            auto elem = ctx.stack().pop();
            if (!elem) [[unlikely]]
                return std::unexpected(elem.error());
            elements.insert(elements.begin(), std::move(*elem));
        }

        auto result = ctx.stack().push(RuntimeValue(elements,
                                                    RuntimeValue::Type::Array));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePushUnit(const PushUnit&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(RuntimeValue(RuntimeValue::Type::Unit));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePushBool(const PushBool& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(RuntimeValue(instr.value));
        if (!result) [[unlikely]] {
            return std::unexpected(result.error());
        }
        return {};
    }

    auto handlePushNone(const PushNone&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        auto result = ctx.stack().push(RuntimeValue(RuntimeValue::Type::None));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

    auto handlePopN(const PopN& instr, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        if (instr.count < 0) [[unlikely]]
            return std::unexpected(
                VMError::InvalidState{"Pop count must be non-negative"});
        
        const auto count = static_cast<size_t>(instr.count);
        if (ctx.stack().size()< count) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        for (size_t i = 0; i < count; ++i) {
            auto result = ctx.stack().pop();
            if (!result) [[unlikely]] {
                return std::unexpected(result.error());
            }
        }
        return {};
    }

    auto handleDup(const Dup&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size()== 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto top = ctx.stack().peek();
        if (!top) [[unlikely]]
            return std::unexpected(top.error());

        auto result = ctx.stack().push(*top);
        if (!result) [[unlikely]]
            return std::unexpected(result.error());
        return {};
    }

}
