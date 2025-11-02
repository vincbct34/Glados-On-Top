/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Instruction Implementation (inline)
*/

#pragma once

#include <format>

namespace rat
{
    // ================================================================
    // Constructor
    // ================================================================

    template <typename TInstructionType>
    Instruction::Instruction(const TInstructionType &instr)
        : _data(instr)
    {
    }

    // ================================================================
    // Type-checking and access methods
    // ================================================================

    template <typename TInstructionType>
    [[nodiscard]] bool Instruction::is(void) const
    {
        return std::holds_alternative<TInstructionType>(this->_data);
    }

    template <typename TInstructionType>
    [[nodiscard]] TInstructionType *Instruction::getIf(void)
    {
        return std::get_if<TInstructionType>(&this->_data);
    }

    template <typename TInstructionType>
    [[nodiscard]] const TInstructionType *Instruction::getIf(void) const
    {
        return std::get_if<TInstructionType>(&this->_data);
    }

    // ================================================================
    // Visitor pattern
    // ================================================================

    template <typename Visitor>
    decltype(auto) Instruction::visit(Visitor &&visitor)
    {
        return std::visit(std::forward<Visitor>(visitor), this->_data);
    }

    template <typename Visitor>
    decltype(auto) Instruction::visit(Visitor &&visitor) const
    {
        return std::visit(std::forward<Visitor>(visitor), this->_data);
    }

    // ================================================================
    // Instruction description
    // ================================================================

    inline std::string Instruction::describe(void) const
    {
        return this->visit([](const auto &instr) -> std::string
                           {
            using T = std::decay_t<decltype(instr)>;

            if constexpr (std::is_same_v<T, PushInt>) {
                return std::format("PushInt {}", instr.value);
            } else if constexpr (std::is_same_v<T, PushFloat>) {
                return std::format("PushFloat {}", instr.value);
            } else if constexpr (std::is_same_v<T, PushString>) {
                return std::format("PushString \"{}\"", instr.value);
            } else if constexpr (std::is_same_v<T, PushAtom>) {
                return std::format("PushAtom :{}", instr.value);
            } else if constexpr (std::is_same_v<T, PushTuple>) {
                return std::format("PushTuple {}", instr.size);
            } else if constexpr (std::is_same_v<T, PushArray>) {
                return std::format("PushArray {}", instr.size);
            } else if constexpr (std::is_same_v<T, PushUnit>) {
                return "PushUnit";
            } else if constexpr (std::is_same_v<T, PopN>) {
                return std::format("PopN {}", instr.count);
            } else if constexpr (std::is_same_v<T, Dup>) {
                return "Dup";
            } else if constexpr (std::is_same_v<T, LoadVar>) {
                return std::format("LoadVar {}", instr.name);
            } else if constexpr (std::is_same_v<T, StoreVar>) {
                return std::format("StoreVar {}", instr.name);
            } else if constexpr (std::is_same_v<T, LoadLocal>) {
                return std::format("LoadLocal {}", instr.name);
            } else if constexpr (std::is_same_v<T, StoreLocal>) {
                return std::format("StoreLocal {}", instr.name);
            } else if constexpr (std::is_same_v<T, Index>) {
                return "Index";
            } else if constexpr (std::is_same_v<T, ArrayLength>) {
                return "ArrayLength";
            } else if constexpr (std::is_same_v<T, InitState>) {
                return std::format("InitState {}", instr.name);
            } else if constexpr (std::is_same_v<T, GetState>) {
                return std::format("GetState {}", instr.name);
            } else if constexpr (std::is_same_v<T, SetState>) {
                return std::format("SetState {}", instr.name);
            } else if constexpr (std::is_same_v<T, Add>) {
                return "Add";
            } else if constexpr (std::is_same_v<T, Sub>) {
                return "Sub";
            } else if constexpr (std::is_same_v<T, Mul>) {
                return "Mul";
            } else if constexpr (std::is_same_v<T, Div>) {
                return "Div";
            } else if constexpr (std::is_same_v<T, Concat>) {
                return "Concat";
            } else if constexpr (std::is_same_v<T, IncVar>) {
                return std::format("IncVar {}", instr.name);
            } else if constexpr (std::is_same_v<T, DecVar>) {
                return std::format("DecVar {}", instr.name);
            } else if constexpr (std::is_same_v<T, IncVarPost>) {
                return std::format("IncVarPost {}", instr.name);
            } else if constexpr (std::is_same_v<T, DecVarPost>) {
                return std::format("DecVarPost {}", instr.name);
            } else if constexpr (std::is_same_v<T, CmpEq>) {
                return "CmpEq";
            } else if constexpr (std::is_same_v<T, CmpNeq>) {
                return "CmpNeq";
            } else if constexpr (std::is_same_v<T, CmpLt>) {
                return "CmpLt";
            } else if constexpr (std::is_same_v<T, CmpLe>) {
                return "CmpLe";
            } else if constexpr (std::is_same_v<T, CmpGt>) {
                return "CmpGt";
            } else if constexpr (std::is_same_v<T, CmpGe>) {
                return "CmpGe";
            } else if constexpr (std::is_same_v<T, LogicAnd>) {
                return "LogicAnd";
            } else if constexpr (std::is_same_v<T, LogicOr>) {
                return "LogicOr";
            } else if constexpr (std::is_same_v<T, LogicNot>) {
                return "LogicNot";
            } else if constexpr (std::is_same_v<T, Negate>) {
                return "Negate";
            } else if constexpr (std::is_same_v<T, PushNone>) {
                return "PushNone";
            } else if constexpr (std::is_same_v<T, PushBool>) {
                return std::format("PushBool {}", instr.value);
            } else if constexpr (std::is_same_v<T, GetField>) {
                return std::format("GetField {}", instr.field);
            } else if constexpr (std::is_same_v<T, PushJust>) {
                return "PushJust";
            } else if constexpr (std::is_same_v<T, PushLeft>) {
                return "PushLeft";
            } else if constexpr (std::is_same_v<T, PushRight>) {
                return "PushRight";
            } else if constexpr (std::is_same_v<T, MaybeBind>) {
                return std::format("MaybeBind {}", instr.var);
            } else if constexpr (std::is_same_v<T, EitherBind>) {
                return std::format("EitherBind {}", instr.var);
            } else if constexpr (std::is_same_v<T, DefineProcess>) {
                return std::format("DefineProcess {}", instr.name);
            } else if constexpr (std::is_same_v<T, CreateInstance>) {
                return std::format("CreateInstance {}", instr.name);
            } else if constexpr (std::is_same_v<T, Send>) {
                return "Send";
            } else if constexpr (std::is_same_v<T, WaitMessage>) {
                return "WaitMessage";
            } else if constexpr (std::is_same_v<T, DefineFunction>) {
                return std::format("DefineFunction {}", instr.name);
            } else if constexpr (std::is_same_v<T, CallFunction>) {
                return std::format("CallFunction {}", instr.name);
            } else if constexpr (std::is_same_v<T, MatchAtom>) {
                return std::format("MatchAtom :{}", instr.atom);
            } else if constexpr (std::is_same_v<T, MatchVar>) {
                return std::format("MatchVar {}", instr.var);
            } else if constexpr (std::is_same_v<T, MatchTuple>) {
                return std::format("MatchTuple {}", instr.size);
            } else if constexpr (std::is_same_v<T, MatchWildcard>) {
                return "MatchWildcard";
            } else if constexpr (std::is_same_v<T, MatchInt>) {
                return std::format("MatchInt {}", instr.value);
            } else if constexpr (std::is_same_v<T, MatchBool>) {
                return std::format("MatchBool {}", instr.value);
            } else if constexpr (std::is_same_v<T, MatchString>) {
                return std::format("MatchString \"{}\"", instr.value);
            } else if constexpr (std::is_same_v<T, ProcessLoop>) {
                return "ProcessLoop";
            } else if constexpr (std::is_same_v<T, Self>) {
                return "Self";
            } else if constexpr (std::is_same_v<T, ExitProcess>) {
                return "ExitProcess";
            } else if constexpr (std::is_same_v<T, StaticCast>) {
                return std::format("StaticCast {}", instr.type);
            } else if constexpr (std::is_same_v<T, ReinterpretCast>) {
                return std::format("ReinterpretCast {}", instr.type);
            } else if constexpr (std::is_same_v<T, ConstCast>) {
                return std::format("ConstCast {}", instr.type);
            } else if constexpr (std::is_same_v<T, Jump>) {
                return std::format("Jump {}", instr.offset);
            } else if constexpr (std::is_same_v<T, JumpIfFalse>) {
                return std::format("JumpIfFalse {}", instr.offset);
            } else if constexpr (std::is_same_v<T, JumpIfTrue>) {
                return std::format("JumpIfTrue {}", instr.offset);
            } else if constexpr (std::is_same_v<T, Label>) {
                return std::format("Label {}", instr.name);
            } else if constexpr (std::is_same_v<T, Call>) {
                return std::format("Call {}", instr.name);
            } else if constexpr (std::is_same_v<T, Return>) {
                return "Return";
            } else if constexpr (std::is_same_v<T, Print>) {
                return "Print";
            } else if constexpr (std::is_same_v<T, Halt>) {
                return "Halt";
            } else {
                return "Unknown";
            } });
    }

} 
