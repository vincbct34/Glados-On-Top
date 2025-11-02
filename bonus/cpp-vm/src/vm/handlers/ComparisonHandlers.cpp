/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Comparison and Logic Instruction Handlers Implementation
*/

#include "ComparisonHandlers.hpp"
#include "core/InstructionStack.hpp"
#include "vm/VariableStore.hpp"
#include "core/RuntimeValue.hpp"
#include "vm/VariableStore.hpp"

namespace rat::handlers
{
    namespace detail
    {
        /**
         * @brief Generic comparison operation handler
         * @tparam Cmp Comparison functor type
         * @param ctx VM execution context
         * @param cmp Comparison operation to perform
         * @param opName Operation name for error messages
         * @return Success or VMError
         */
        template<typename Cmp>
        auto comparison(VMContext &ctx, Cmp cmp, const char *opName)
            -> std::expected<void, VMError>
        {
            if (ctx.stack().size() < 2) [[unlikely]]
                return std::unexpected(VMError{VMError::StackUnderflow{}});

            auto right = ctx.stack().pop();
            auto left = ctx.stack().pop();
            
            if (!right || !left) [[unlikely]]
                return std::unexpected(VMError{VMError::StackUnderflow{}});

            bool result;
            
            if (left->type() == RuntimeValue::Type::I32 &&
                right->type() == RuntimeValue::Type::I32) [[likely]] {
                auto lval = left->getIf<std::int32_t>();
                auto rval = right->getIf<std::int32_t>();
                result = cmp(lval->get(), rval->get());
            } else if (left->type() == RuntimeValue::Type::F64 &&
                     right->type() == RuntimeValue::Type::F64) {
                auto lval = left->getIf<double>();
                auto rval = right->getIf<double>();
                result = cmp(lval->get(), rval->get());
            } else if ((left->type() == RuntimeValue::Type::I32 ||
                        left->type() == RuntimeValue::Type::F64) 
                     && (right->type() == RuntimeValue::Type::I32 ||
                         right->type() == RuntimeValue::Type::F64)) {
                double lval = left->type() == RuntimeValue::Type::I32 
                            ? static_cast<double>(left->getIf<std::int32_t>()->get()) 
                            : left->getIf<double>()->get();
                double rval = right->type() == RuntimeValue::Type::I32 
                            ? static_cast<double>(right->getIf<std::int32_t>()->get()) 
                            : right->getIf<double>()->get();
                result = cmp(lval, rval);
            } else if (left->type() == RuntimeValue::Type::String 
                     && right->type() == RuntimeValue::Type::String) {
                auto lval = left->getIf<std::string>();
                auto rval = right->getIf<std::string>();
                result = cmp(lval->get(), rval->get());
            }
            else if (left->type() == RuntimeValue::Type::Bool 
                     && right->type() == RuntimeValue::Type::Bool) {
                auto lval = left->getIf<bool>();
                auto rval = right->getIf<bool>();
                result = cmp(lval->get(), rval->get());
            } else [[unlikely]]
                return std::unexpected(VMError{VMError::InvalidOperand{
                    std::string("Invalid types for ") + opName}});

            auto pushResult = ctx.stack().push(RuntimeValue(result));

            if (!pushResult) [[unlikely]]
                return std::unexpected(pushResult.error());

            return {};
        }
    }

    auto handleCmpEq(const CmpEq&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        return detail::comparison(ctx, 
            [](const auto& a, const auto& b) { return a == b; }, "equality");
    }

    auto handleCmpNeq(const CmpNeq&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        return detail::comparison(ctx, 
            [](const auto& a, const auto& b) { return a != b; }, "inequality");
    }

    auto handleCmpLt(const CmpLt&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        return detail::comparison(ctx, 
            [](const auto& a, const auto& b) { return a < b; }, "less than");
    }

    auto handleCmpLe(const CmpLe&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        return detail::comparison(ctx, 
            [](const auto& a, const auto& b) { return a <= b; }, "less than or equal");
    }

    auto handleCmpGt(const CmpGt&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        return detail::comparison(ctx, 
            [](const auto& a, const auto& b) { return a > b; }, "greater than");
    }

    auto handleCmpGe(const CmpGe&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        return detail::comparison(ctx, 
            [](const auto& a, const auto& b) { return a >= b; }, "greater than or equal");
    }

    auto handleLogicAnd(const LogicAnd&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() < 2) [[unlikely]]
            return std::unexpected(VMError{VMError::StackUnderflow{}});

        auto right = ctx.stack().pop();
        auto left = ctx.stack().pop();
        
        if (!right || !left) [[unlikely]]
            return std::unexpected(VMError{VMError::StackUnderflow{}});

        if (left->type() != RuntimeValue::Type::Bool 
            || right->type() != RuntimeValue::Type::Bool) [[unlikely]]
            return std::unexpected(VMError{VMError::InvalidOperand{
                "Logical AND requires booleans"}});

        auto lval = left->getIf<bool>();
        auto rval = right->getIf<bool>();
        bool result = lval->get() && rval->get();
        auto pushResult = ctx.stack().push(RuntimeValue(result));

        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        return {};
    }

    auto handleLogicOr(const LogicOr&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() < 2) [[unlikely]]
            return std::unexpected(VMError{VMError::StackUnderflow{}});

        auto right = ctx.stack().pop();
        auto left = ctx.stack().pop();
        
        if (!right || !left) [[unlikely]]
            return std::unexpected(VMError{VMError::StackUnderflow{}});

        if (left->type() != RuntimeValue::Type::Bool 
            || right->type() != RuntimeValue::Type::Bool) [[unlikely]]
            return std::unexpected(VMError{VMError::InvalidOperand{
                "Logical OR requires booleans"}});

        auto lval = left->getIf<bool>();
        auto rval = right->getIf<bool>();
        bool result = lval->get() || rval->get();
        auto pushResult = ctx.stack().push(RuntimeValue(result));

        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        return {};
    }

    auto handleLogicNot(const LogicNot&, VMContext& ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError{VMError::StackUnderflow{}});

        auto val = ctx.stack().pop();

        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        if (val->type() != RuntimeValue::Type::Bool) [[unlikely]]
            return std::unexpected(VMError{VMError::InvalidOperand{
                "Logical NOT requires boolean"}});

        auto v = val->getIf<bool>();
        bool result = !v->get();
        auto pushResult = ctx.stack().push(RuntimeValue(result));

        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        return {};
    }

}
