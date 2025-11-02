/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Instruction Handler Interface - Strategy Pattern
*/

#pragma once

#include <expected>
#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"

namespace rat
{
    // Forward declarations
    class VMContext;

    /**
     * @class IInstructionHandler
     * @brief Interface for instruction handlers using Strategy pattern.
     * Enables SOLID principles and optimized dispatch.
     *
     * This replaces the std::function-based approach with direct virtual dispatch,
     * which is faster and uses less memory.
     */
    class IInstructionHandler
    {
    public:
        virtual ~IInstructionHandler() = default;

        // Pure virtual dispatch methods for each instruction category
        virtual auto execute(const PushInt &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushFloat &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushString &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushAtom &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushTuple &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushArray &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushUnit &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PopN &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Dup &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const LoadVar &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const StoreVar &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const LoadLocal &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const StoreLocal &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const Index &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const ArrayLength &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const InitState &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const GetState &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const SetState &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const Add &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Sub &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Mul &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Div &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Concat &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const IncVar &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const DecVar &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const IncVarPost &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const DecVarPost &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const CmpEq &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const CmpNeq &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const CmpLt &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const CmpLe &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const CmpGt &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const CmpGe &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const LogicAnd &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const LogicOr &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const LogicNot &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Negate &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const PushNone &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushBool &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const GetField &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const PushJust &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushLeft &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const PushRight &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const MaybeBind &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const EitherBind &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const DefineProcess &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const CreateInstance &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Send &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const WaitMessage &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const DefineFunction &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const CallFunction &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const MatchAtom &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const MatchVar &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const MatchTuple &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const MatchWildcard &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const MatchInt &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const MatchBool &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const MatchString &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const ProcessLoop &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Self &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const ExitProcess &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const StaticCast &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const ReinterpretCast &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const ConstCast &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const Jump &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const JumpIfFalse &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const JumpIfTrue &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Label &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Call &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Return &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;

        virtual auto execute(const Print &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
        virtual auto execute(const Halt &instr, VMContext &ctx) -> std::expected<void, VMError> = 0;
    };

}
