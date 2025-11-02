/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Arithmetic Instruction Handler
*/

#include "ArithmeticHandlers.hpp"
#include "core/InstructionStack.hpp"
#include "core/RuntimeValue.hpp"
#include "vm/VariableStore.hpp"
#include <cmath>

namespace rat::handlers
{
    namespace detail
    {
        /**
         * @brief Generic binary arithmetic operation handler
         * @tparam Op Operation functor type
         * @param ctx VM execution context
         * @param op Binary operation to perform
         * @param opName Operation name for error messages
         * @return Success or VMError
         */
        template<typename Op>
        auto binaryArithmetic(VMContext &ctx, Op op, const char* opName)
            -> std::expected<void, VMError>
        {
            if (ctx.stack().size()< 2) [[unlikely]] {
                return std::unexpected(VMError::StackUnderflow{});
            }

            auto right = ctx.stack().pop();
            auto left = ctx.stack().pop();
            
            if (!right || !left) [[unlikely]] {
                return std::unexpected(VMError::StackUnderflow{});
            }

            RuntimeValue result(std::int32_t{0});
            
            if (left->type() == RuntimeValue::Type::I32 
                && right->type() == RuntimeValue::Type::I32) [[likely]] {
                auto lval = left->getIf<std::int32_t>();
                auto rval = right->getIf<std::int32_t>();
                if (!lval || !rval) [[unlikely]] {
                    return std::unexpected(VMError::InvalidOperand{"Expected int32"});
                }
                result = RuntimeValue(static_cast<std::int32_t>(
                    op(lval->get(), rval->get())));
            } else if (left->type() == RuntimeValue::Type::F64 &&
                        right->type() == RuntimeValue::Type::F64) {
                auto lval = left->getIf<double>();
                auto rval = right->getIf<double>();
                if (!lval || !rval) [[unlikely]] {
                    return std::unexpected(VMError::InvalidOperand{"Expected float64"});
                }
                result = RuntimeValue(op(lval->get(), rval->get()));
            } else if ((left->type() == RuntimeValue::Type::I32 ||
                        left->type() == RuntimeValue::Type::F64) &&
                       (right->type() == RuntimeValue::Type::I32 ||
                        right->type() == RuntimeValue::Type::F64)) {
                double lval = left->type() == RuntimeValue::Type::I32 
                            ? static_cast<double>(left->getIf<std::int32_t>()->get()) 
                            : left->getIf<double>()->get();
                double rval = right->type() == RuntimeValue::Type::I32 
                            ? static_cast<double>(right->getIf<std::int32_t>()->get()) 
                            : right->getIf<double>()->get();
                result = RuntimeValue(op(lval, rval));
            } else [[unlikely]]
                return std::unexpected(VMError::InvalidOperand{
                    std::format("Invalid types for {}", opName)});

            auto pushResult = ctx.stack().push(std::move(result));
            if (!pushResult) [[unlikely]]
                return std::unexpected(pushResult.error());
            return {};
        }
    }

    auto handleAdd(const Add &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        return detail::binaryArithmetic(ctx, 
            [](auto a, auto b) { return a + b; }, "add");
    }

    auto handleSub(const Sub&, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        return detail::binaryArithmetic(ctx, 
            [](auto a, auto b) { return a - b; }, "subtract");
    }

    auto handleMul(const Mul&, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        return detail::binaryArithmetic(ctx, 
            [](auto a, auto b) { return a * b; }, "multiply");
    }

    auto handleDiv(const Div&, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() < 2) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto right = ctx.stack().pop();
        auto left = ctx.stack().pop();
        
        if (!right || !left) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        if (right->type() == RuntimeValue::Type::I32) {
            auto rval = right->getIf<std::int32_t>();
            if (rval && rval->get()== 0) [[unlikely]] {
                return std::unexpected(VMError::DivisionByZero{});
            }
        }
        if (right->type()== RuntimeValue::Type::F64) {
            auto rval = right->getIf<double>();
            if (rval && std::abs(rval->get()) < 1e-10) [[unlikely]] {
                return std::unexpected(VMError::DivisionByZero{});
            }
        }

        RuntimeValue result(std::int32_t{0});
        
        if (left->type() == RuntimeValue::Type::I32 
            && right->type() == RuntimeValue::Type::I32) [[likely]] {
            auto lval = left->getIf<std::int32_t>();
            auto rval = right->getIf<std::int32_t>();
            result = RuntimeValue(lval->get() / rval->get());
        } else if (left->type() == RuntimeValue::Type::F64 
                 && right->type() == RuntimeValue::Type::F64) {
            auto lval = left->getIf<double>();
            auto rval = right->getIf<double>();
            result = RuntimeValue(lval->get() / rval->get());
        } else if ((left->type() == RuntimeValue::Type::I32  ||
                  left->type() == RuntimeValue::Type::F64) &&
                 (right->type() == RuntimeValue::Type::I32 ||
                  right->type() == RuntimeValue::Type::F64)) {
            double lval = left->type() == RuntimeValue::Type::I32 
                        ? static_cast<double>(
                            left->getIf<std::int32_t>()->get()) 
                        : left->getIf<double>()->get();
            double rval = right->type() == RuntimeValue::Type::I32 
                        ? static_cast<double>(
                            right->getIf<std::int32_t>()->get()) 
                        : right->getIf<double>()->get();
            result = RuntimeValue(lval / rval);
        } else [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Invalid types for division"});

        auto pushResult = ctx.stack().push(std::move(result));
        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());
        return {};
    }

    auto handleNegate(const Negate&, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();

        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        RuntimeValue result(std::int32_t{0});

        if (val->type() == RuntimeValue::Type::I32) [[likely]] {
            auto v = val->getIf<std::int32_t>();
            result = RuntimeValue(-v->get());
        }
        else if (val->type() == RuntimeValue::Type::F64) {
            auto v = val->getIf<double>();
            result = RuntimeValue(-v->get());
        } else [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Cannot negate non-numeric value"});

        auto pushResult = ctx.stack().push(std::move(result));

        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        return {};
    }

    auto handleConcat(const Concat&, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() < 2) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto right = ctx.stack().pop();
        auto left = ctx.stack().pop();
        
        if (!right || !left) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        if (left->type() != RuntimeValue::Type::String 
            || right->type() != RuntimeValue::Type::String) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Concat requires strings"});

        auto lstr = left->getIf<std::string>();
        auto rstr = right->getIf<std::string>();
        std::string result = lstr->get() + rstr->get();
        auto pushResult = ctx.stack().push(RuntimeValue(std::move(result)));

        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        return {};
    }

    auto handleIncVar(const IncVar &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto val = ctx.globals().get(instr.name);
        bool inGlobals = val.has_value();

        if (!val)
            val = ctx.locals().get(instr.name);

        if (!val) [[unlikely]]
            return std::unexpected(VMError::VariableNotFound{instr.name});

        if (val->type() != RuntimeValue::Type::I32) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Inc requires int32"});

        auto v = val->getIf<std::int32_t>();
        auto newVal = RuntimeValue(v->get() + 1);
        auto storeResult = inGlobals
            ? ctx.globals().set(instr.name, std::move(newVal))
            : ctx.locals().set(instr.name, std::move(newVal));
            
        if (!storeResult) [[unlikely]]
            return std::unexpected(storeResult.error());

        return {};
    }

    auto handleDecVar(const DecVar &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto val = ctx.globals().get(instr.name);
        bool inGlobals = val.has_value();

        if (!val)
            val = ctx.locals().get(instr.name);

        if (!val) [[unlikely]]
            return std::unexpected(VMError::VariableNotFound{instr.name});

        if (val->type() != RuntimeValue::Type::I32) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Dec requires int32"});

        auto v = val->getIf<std::int32_t>();
        auto newVal = RuntimeValue(v->get() - 1);
        auto storeResult = inGlobals
                         ? ctx.globals().set(instr.name, std::move(newVal))
                         : ctx.locals().set(instr.name, std::move(newVal));
            
        if (!storeResult) [[unlikely]]
            return std::unexpected(storeResult.error());

        return {};
    }

    auto handleIncVarPost(const IncVarPost &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto val = ctx.globals().get(instr.name);
        bool inGlobals = val.has_value();

        if (!val)
            val = ctx.locals().get(instr.name);

        if (!val) [[unlikely]]
            return std::unexpected(VMError::VariableNotFound{instr.name});

        if (val->type() != RuntimeValue::Type::I32) [[unlikely]]
            return std::unexpected(
                VMError::InvalidOperand{"Inc requires int32"});

        auto pushResult = ctx.stack().push(*val);

        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        auto v = val->getIf<std::int32_t>();
        auto newVal = RuntimeValue(v->get() + 1);
        auto storeResult = inGlobals
                         ? ctx.globals().set(instr.name, std::move(newVal))
                         : ctx.locals().set(instr.name, std::move(newVal));
            
        if (!storeResult) [[unlikely]]
            return std::unexpected(storeResult.error());

        return {};
    }

    auto handleDecVarPost(const DecVarPost &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto val = ctx.globals().get(instr.name);
        bool inGlobals = val.has_value();

        if (!val)
            val = ctx.locals().get(instr.name);

        if (!val) [[unlikely]]
            return std::unexpected(VMError::VariableNotFound{instr.name});

        if (val->type() != RuntimeValue::Type::I32) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Dec requires int32"});

        auto pushResult = ctx.stack().push(*val);

        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        auto v = val->getIf<std::int32_t>();
        auto newVal = RuntimeValue(v->get() - 1);
        auto storeResult = inGlobals
                         ? ctx.globals().set(instr.name, std::move(newVal))
                         : ctx.locals().set(instr.name, std::move(newVal));
            
        if (!storeResult) [[unlikely]]
            return std::unexpected(storeResult.error());

        return {};
    }

}
