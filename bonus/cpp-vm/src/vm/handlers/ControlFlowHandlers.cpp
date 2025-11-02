/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Control Flow Instruction Handlers Implementation
*/

#include "ControlFlowHandlers.hpp"

#include "core/InstructionStack.hpp"
#include "core/RuntimeValue.hpp"
#include "vm/FunctionManager.hpp"
#include "vm/ProcessManager.hpp"
#include "vm/VariableStore.hpp"

#include <iostream>

namespace rat::handlers
{
    auto handleJump(const Jump &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        const auto newPC = static_cast<size_t>(
            static_cast<int64_t>(ctx.programCounter()) + instr.offset);
        ctx.programCounter() = newPC;
        return {};
    }

    auto handleJumpIfFalse(const JumpIfFalse &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto cond = ctx.stack().pop();
        if (!cond) [[unlikely]]
            return std::unexpected(cond.error());

        if (cond->type() != RuntimeValue::Type::Bool) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Jump condition must be boolean"});

        const auto boolVal = cond->getIf<bool>();
        if (!boolVal->get()) [[likely]] {
            const auto newPC = static_cast<size_t>(
                static_cast<int64_t>(ctx.programCounter()) + instr.offset);
            ctx.programCounter() = newPC;
        }

        return {};
    }

    auto handleJumpIfTrue(const JumpIfTrue &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto cond = ctx.stack().pop();
        if (!cond) [[unlikely]]
            return std::unexpected(cond.error());

        if (cond->type() != RuntimeValue::Type::Bool) [[unlikely]]
            return std::unexpected(VMError::InvalidOperand{
                "Jump condition must be boolean"});

        const auto boolVal = cond->getIf<bool>();
        if (boolVal->get()) {
            const auto newPC = static_cast<size_t>(
                static_cast<int64_t>(ctx.programCounter()) + instr.offset);
            ctx.programCounter() = newPC;
        }

        return {};
    }

    auto handleLabel(const Label &, VMContext &) 
        -> std::expected<void, VMError>
    {
        return {};
    }

    auto handleCall(const Call &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto &funcMgr = ctx.functionManager();
        auto funcDef = static_cast<const FunctionManager &>(funcMgr)
            .getFunction(instr.name);
        
        if (!funcDef) [[unlikely]]
            return std::unexpected(funcDef.error());

        auto pushResult = funcMgr.pushCallFrame(
            instr.name,
            ctx.programCounter() + 1,
            ctx.locals());
        
        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        const FunctionDef *func = *funcDef;
        if (!func->body.instructions.empty())
            ctx.programCounter() = 0;
        
        return {};
    }

    auto handleReturn(const Return &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto frame = ctx.functionManager().popCallFrame();
        if (!frame) [[unlikely]]
            return std::unexpected(frame.error());

        ctx.programCounter() = frame->returnPC;
        ctx.locals() = std::move(frame->savedLocals);
        
        return {};
    }

    auto handleHalt(const Halt &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        ctx.setRunning(false);
        return {};
    }

    auto handlePrint(const Print &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        std::cout << val->toString() << std::endl;
        return {};
    }

    auto handleDefineFunction(const DefineFunction &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        std::vector<std::string> params;
        std::vector<Instruction> body;
        
        return ctx.functionManager().defineFunction(instr.name, params, body);
    }

    auto handleCallFunction(const CallFunction &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto &funcMgr = ctx.functionManager();
        auto funcDef = static_cast<const FunctionManager &>(funcMgr)
            .getFunction(instr.name);
        if (!funcDef) [[unlikely]]
            return std::unexpected(funcDef.error());

        auto pushResult = funcMgr.pushCallFrame(
            instr.name,
            ctx.programCounter() + 1,
            ctx.locals());
        
        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());

        ctx.locals().clear();
        
        const FunctionDef *func = *funcDef;
        if (!func->body.instructions.empty())
            ctx.programCounter() = 0;
        
        return {};
    }

    auto handleMatchAtom(const MatchAtom &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().peek();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        bool matched = false;
        if (val->get().type() == RuntimeValue::Type::Atom) {
            auto atomVal = val->get().getIf<std::string>();
            if (atomVal && atomVal->get() == instr.atom) {
                auto popRes = ctx.stack().pop();
                if (!popRes) [[unlikely]]
                    return std::unexpected(popRes.error());
                matched = true;
            }
        }

        if (!matched) {
            auto const newPC = static_cast<size_t>(
                static_cast<int64_t>(ctx.programCounter()) + instr.offset);
            ctx.programCounter() = newPC;
        }
        
        return {};
    }

    auto handleMatchVar(const MatchVar &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        return ctx.locals().set(instr.var, std::move(*val));
    }

    auto handleMatchTuple(const MatchTuple &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().peek();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        bool matched = false;
        if (val->get().type() == RuntimeValue::Type::Tuple) {
            auto tupleVal = val->get().getIf<std::vector<RuntimeValue>>();
            if (tupleVal 
                && tupleVal->get().size() == static_cast<size_t>(instr.size)) {
                auto popRes = ctx.stack().pop();
                if (!popRes) [[unlikely]]
                    return std::unexpected(popRes.error());

                const std::vector<RuntimeValue> &elements = tupleVal->get();
                for (auto it = elements.rbegin(); it != elements.rend(); ++it) {
                    auto pushRes = ctx.stack().push(*it);
                    if (!pushRes) [[unlikely]]
                        return std::unexpected(pushRes.error());
                }
                matched = true;
            }
        }

        if (!matched) {
            const auto newPC = static_cast<size_t>(
                static_cast<int64_t>(ctx.programCounter()) + instr.offset);
            ctx.programCounter() = newPC;
        }

        return {};
    }

    auto handleMatchWildcard(const MatchWildcard &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto popRes = ctx.stack().pop();
        if (!popRes) [[unlikely]]
            return std::unexpected(popRes.error());

        return {};
    }

    auto handleMatchInt(const MatchInt &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().peek();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        bool matched = false;
        if (val->get().type() == RuntimeValue::Type::I32) {
            auto intVal = val->get().getIf<std::int32_t>();
            if (intVal && intVal->get() == instr.value) {
                auto popRes = ctx.stack().pop();
                if (!popRes) [[unlikely]]
                    return std::unexpected(popRes.error());
                matched = true;
            }
        }

        if (!matched) {
            const auto newPC = static_cast<size_t>(
                static_cast<int64_t>(ctx.programCounter()) + instr.offset);
            ctx.programCounter() = newPC;
        }

        return {};
    }

    auto handleMatchBool(const MatchBool &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().peek();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        bool matched = false;
        if (val->get().type() == RuntimeValue::Type::Bool) {
            auto boolVal = val->get().getIf<bool>();
            if (boolVal && boolVal->get() == instr.value) {
                auto popRes = ctx.stack().pop();
                if (!popRes) [[unlikely]]
                    return std::unexpected(popRes.error());
                matched = true;
            }
        }

        if (!matched) {
            const auto newPC = static_cast<size_t>(
                static_cast<int64_t>(ctx.programCounter()) + instr.offset);
            ctx.programCounter() = newPC;
        }

        return {};
    }

    auto handleMatchString(const MatchString &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().peek();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        bool matched = false;
        if (val->get().type() == RuntimeValue::Type::String) {
            auto strVal = val->get().getIf<std::string>();
            if (strVal && strVal->get() == instr.value) {
                auto popRes = ctx.stack().pop();
                if (!popRes) [[unlikely]]
                    return std::unexpected(popRes.error());
                matched = true;
            }
        }

        if (!matched) {
            const auto newPC = static_cast<size_t>(
                static_cast<int64_t>(ctx.programCounter()) + instr.offset);
            ctx.programCounter() = newPC;
        }

        return {};
    }

    auto handlePushJust(const PushJust &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});
        
        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());
        
        auto justVal = RuntimeValue(
            std::make_unique<RuntimeValue>(std::move(*val)),
            RuntimeValue::Type::Just);
        return ctx.stack().push(std::move(justVal));
    }

    auto handlePushLeft(const PushLeft &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});
        
        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());
        
        auto leftVal = RuntimeValue(
            std::make_unique<RuntimeValue>(std::move(*val)),
            RuntimeValue::Type::Left);
        return ctx.stack().push(std::move(leftVal));
    }

    auto handlePushRight(const PushRight &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});
        
        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());
        
        auto rightVal = RuntimeValue(
            std::make_unique<RuntimeValue>(std::move(*val)),
            RuntimeValue::Type::Right);
        return ctx.stack().push(std::move(rightVal));
    }

    auto handleMaybeBind(const MaybeBind &, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto maybeVal = ctx.stack().pop();
        if (!maybeVal) [[unlikely]]
            return std::unexpected(maybeVal.error());

        if (maybeVal->type() == RuntimeValue::Type::Just) {
            auto wrappedPtr = maybeVal->getIf<std::unique_ptr<RuntimeValue>>();
            if (wrappedPtr && wrappedPtr->get())
                return ctx.stack().push(RuntimeValue(*wrappedPtr->get()));
            return std::unexpected(VMError::InvalidState{"Just value is null"});
        }
        
        if (maybeVal->type() == RuntimeValue::Type::None)
            return ctx.stack().push(std::move(*maybeVal));
        
        return std::unexpected(VMError::InvalidState{
            "MAYBE_BIND requires Maybe value (Just or None)"});
    }

    auto handleEitherBind(const EitherBind &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto eitherVal = ctx.stack().pop();
        if (!eitherVal) [[unlikely]]
            return std::unexpected(eitherVal.error());

        if (eitherVal->type() == RuntimeValue::Type::Right) {
            auto wrappedPtr = eitherVal->getIf<std::unique_ptr<RuntimeValue>>();
            if (wrappedPtr && wrappedPtr->get())
                return ctx.stack().push(RuntimeValue(*wrappedPtr->get()));
            return std::unexpected(VMError::InvalidState{"Right value is null"});
        }
        
        if (eitherVal->type() == RuntimeValue::Type::Left)
            return ctx.stack().push(std::move(*eitherVal));
        
        return std::unexpected(VMError::InvalidState{
            "EITHER_BIND requires Either value (Left or Right)"});
    }

    auto handleDefineProcess(const DefineProcess &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto &mgr = ctx.processManager();
        std::vector<std::string> params;
        std::vector<Instruction> body;
        
        return mgr.defineProcess(instr.name, params, body);
    }

    auto handleCreateInstance(const CreateInstance &instr, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto &mgr = ctx.processManager();
        std::vector<RuntimeValue> args;
        
        auto pidResult = mgr.spawn(instr.name, args);
        if (!pidResult) [[unlikely]]
            return std::unexpected(pidResult.error());
        
        auto pushResult = ctx.stack().push(RuntimeValue(*pidResult));
        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());
        
        return {};
    }

    auto handleSend(const Send &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() < 2) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto message = ctx.stack().pop();
        if (!message) [[unlikely]]
            return std::unexpected(message.error());

        auto pidVal = ctx.stack().pop();
        if (!pidVal) [[unlikely]]
            return std::unexpected(pidVal.error());

        if (pidVal->type() != RuntimeValue::Type::Pid) [[unlikely]]
            return std::unexpected(VMError::TypeMismatch{"Pid", "Unknown"});

        auto pid = pidVal->getIf<std::int64_t>();
        if (!pid) [[unlikely]]
            return std::unexpected(VMError::InvalidState{"Invalid PID"});

        auto &mgr = ctx.processManager();
        return mgr.send(
            process::ProcessIdentity(pid->get()),
            std::move(*message));
    }

    auto handleWaitMessage(const WaitMessage &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto &mgr = ctx.processManager();
        auto currentPid = mgr.getCurrentPid();
        auto msgResult = mgr.receive(currentPid);
        if (!msgResult) [[unlikely]]
            return std::unexpected(msgResult.error());
        
        auto pushResult = ctx.stack().push(std::move(*msgResult));
        if (!pushResult) [[unlikely]]
            return std::unexpected(pushResult.error());
        
        return {};
    }

    auto handleProcessLoop(const ProcessLoop &, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        ctx.programCounter() = 0;
        return {};
    }

    auto handleSelf(const Self &, VMContext &ctx) 
        -> std::expected<void, VMError>
    {
        auto &mgr = ctx.processManager();
        auto pid = mgr.getCurrentPid();
        auto result = ctx.stack().push(RuntimeValue(pid));
        if (!result) [[unlikely]]
            return std::unexpected(result.error());

        return {};
    }

    auto handleExitProcess(const ExitProcess &, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        auto &mgr = ctx.processManager();
        auto currentPid = mgr.getCurrentPid();
        
        auto terminateResult = mgr.terminate(currentPid);
        if (!terminateResult) [[unlikely]]
            return std::unexpected(terminateResult.error());
        
        ctx.setRunning(false);
        return {};
    }

    auto handleStaticCast(const StaticCast &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        const std::string &targetType = instr.type;
        
        if (targetType == "Int" || targetType == "int") {
            if (val->type() == RuntimeValue::Type::F32) {
                auto fval = val->getIf<float>();
                if (fval)
                    return ctx.stack().push(
                        RuntimeValue(static_cast<std::int32_t>(fval->get())));
            } else if (val->type() == RuntimeValue::Type::F64) {
                auto fval = val->getIf<double>();
                if (fval)
                    return ctx.stack().push(
                        RuntimeValue(static_cast<std::int32_t>(fval->get())));
            } else if (val->type() == RuntimeValue::Type::Bool) {
                auto bval = val->getIf<bool>();
                if (bval)
                    return ctx.stack().push(
                        RuntimeValue(static_cast<std::int32_t>(
                            bval->get() ? 1 : 0)));
            } else if (val->type() == RuntimeValue::Type::I32) {
                return ctx.stack().push(std::move(*val));
            }
        } else if (targetType == "Float" || targetType == "float") {
            if (val->type() == RuntimeValue::Type::I32) {
                auto ival = val->getIf<std::int32_t>();
                if (ival)
                    return ctx.stack().push(
                        RuntimeValue(static_cast<double>(ival->get())));
            } else if (val->type() == RuntimeValue::Type::I64) {
                auto ival = val->getIf<std::int64_t>();
                if (ival)
                    return ctx.stack().push(
                        RuntimeValue(static_cast<double>(ival->get())));
            } else if (val->type() == RuntimeValue::Type::F32) {
                auto fval = val->getIf<float>();
                if (fval)
                    return ctx.stack().push(
                        RuntimeValue(static_cast<double>(fval->get())));
            } else if (val->type() == RuntimeValue::Type::F64) {
                return ctx.stack().push(std::move(*val));
            }
        } else if (targetType == "Bool" || targetType == "bool") {
            if (val->type() == RuntimeValue::Type::I32) {
                auto ival = val->getIf<std::int32_t>();
                if (ival)
                    return ctx.stack().push(
                        RuntimeValue(ival->get() != 0));
            } else if (val->type() == RuntimeValue::Type::I64) {
                auto ival = val->getIf<std::int64_t>();
                if (ival)
                    return ctx.stack().push(
                        RuntimeValue(ival->get() != 0));
            } else if (val->type() == RuntimeValue::Type::Bool) {
                return ctx.stack().push(std::move(*val));
            }
        } else if (targetType == "String" || targetType == "string") {
            return ctx.stack().push(RuntimeValue(val->toString()));
        }
        
        return std::unexpected(VMError::InvalidCast{"Unknown", targetType});
    }

    auto handleReinterpretCast(const ReinterpretCast &instr, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        auto val = ctx.stack().pop();
        if (!val) [[unlikely]]
            return std::unexpected(val.error());

        [[maybe_unused]] const std::string &targetType = instr.type;
        
        return ctx.stack().push(std::move(*val));
    }

    auto handleConstCast(const ConstCast &, VMContext &ctx)
        -> std::expected<void, VMError>
    {
        if (ctx.stack().size() == 0) [[unlikely]]
            return std::unexpected(VMError::StackUnderflow{});

        return {};
    }

}
