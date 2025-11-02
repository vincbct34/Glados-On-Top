/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Control Flow Instruction Handlers
*/

#pragma once

#include "common/ErrorCode.hpp"
#include "vm/Instruction.hpp"
#include "vm/VMContext.hpp"

#include <expected>

namespace rat::handlers
{
    /**
     * @brief Unconditionally jump to an instruction at a relative offset.
     * @param instr The jump instruction containing the offset.
     * @param ctx The VM context.
     * @return Success or error if the jump target is invalid.
     */
    [[nodiscard]]
    auto handleJump(const Jump& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Pop a boolean from the stack and jump if it is false.
     * @param instr The conditional jump instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow or type mismatch.
     */
    [[nodiscard]]
    auto handleJumpIfFalse(const JumpIfFalse& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Pop a boolean from the stack and jump if it is true.
     * @param instr The conditional jump instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow or type mismatch.
     */
    [[nodiscard]]
    auto handleJumpIfTrue(const JumpIfTrue& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Mark a label position (no-op in execution).
     * @param instr The label instruction.
     * @param ctx The VM context.
     * @return Always succeeds.
     */
    [[nodiscard]]
    auto handleLabel(const Label& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Call a function by label name.
     * @param instr The call instruction.
     * @param ctx The VM context.
     * @return Error as label-based calls are not yet implemented.
     */
    [[nodiscard]]
    auto handleCall(const Call& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Return from a function call and restore the call frame.
     * @param instr The return instruction.
     * @param ctx The VM context.
     * @return Success or error if the call stack is empty.
     */
    [[nodiscard]]
    auto handleReturn(const Return& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Halt the VM execution.
     * @param instr The halt instruction.
     * @param ctx The VM context.
     * @return Always succeeds.
     */
    [[nodiscard]]
    auto handleHalt(const Halt& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Print the top stack value to standard output.
     * @param instr The print instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handlePrint(const Print& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Define a new function.
     * @param instr The function definition instruction.
     * @param ctx The VM context.
     * @return Success or error if definition fails.
     */
    [[nodiscard]]
    auto handleDefineFunction(const DefineFunction& instr,
        VMContext& ctx) -> std::expected<void, VMError>;

    /**
     * @brief Call a defined function by name.
     * @param instr The function call instruction.
     * @param ctx The VM context.
     * @return Success or error if the function is not found.
     */
    [[nodiscard]]
    auto handleCallFunction(const CallFunction& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Match an atom value and jump if not matched.
     * @param instr The atom pattern matching instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleMatchAtom(const MatchAtom& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Bind a stack value to a variable.
     * @param instr The variable binding instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleMatchVar(const MatchVar& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Match a tuple and decompose it onto the stack.
     * @param instr The tuple pattern matching instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow or size mismatch.
     */
    [[nodiscard]]
    auto handleMatchTuple(const MatchTuple& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Match any value (wildcard pattern).
     * @param instr The wildcard matching instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleMatchWildcard(const MatchWildcard& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Match an integer value and jump if not matched.
     * @param instr The integer pattern matching instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleMatchInt(const MatchInt& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Match a boolean value and jump if not matched.
     * @param instr The boolean pattern matching instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleMatchBool(const MatchBool& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Match a string value and jump if not matched.
     * @param instr The string pattern matching instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleMatchString(const MatchString& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Wrap a value in a Just constructor (Maybe monad).
     * @param instr The Just instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handlePushJust(const PushJust& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Wrap a value in a Left constructor (Either monad).
     * @param instr The Left instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handlePushLeft(const PushLeft& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Wrap a value in a Right constructor (Either monad).
     * @param instr The Right instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handlePushRight(const PushRight& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Unwrap a Maybe value for monadic bind.
     * @param instr The Maybe bind instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow or invalid type.
     */
    [[nodiscard]]
    auto handleMaybeBind(const MaybeBind& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Unwrap an Either value for monadic bind.
     * @param instr The Either bind instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow or invalid type.
     */
    [[nodiscard]]
    auto handleEitherBind(const EitherBind& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Define a new process type.
     * @param instr The process definition instruction.
     * @param ctx The VM context.
     * @return Success or error if definition fails.
     */
    [[nodiscard]]
    auto handleDefineProcess(const DefineProcess& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Spawn a new process instance and push its PID.
     * @param instr The instance creation instruction.
     * @param ctx The VM context.
     * @return Success or error if spawn fails.
     */
    [[nodiscard]]
    auto handleCreateInstance(const CreateInstance& instr,
        VMContext& ctx) -> std::expected<void, VMError>;

    /**
     * @brief Send a message to a process by PID.
     * @param instr The send instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow or invalid PID.
     */
    [[nodiscard]]
    auto handleSend(const Send& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Wait for and receive a message from the mailbox.
     * @param instr The wait message instruction.
     * @param ctx The VM context.
     * @return Success or error if no message available.
     */
    [[nodiscard]]
    auto handleWaitMessage(const WaitMessage& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Reset the program counter to loop the process.
     * @param instr The process loop instruction.
     * @param ctx The VM context.
     * @return Always succeeds.
     */
    [[nodiscard]]
    auto handleProcessLoop(const ProcessLoop& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Push the current process ID onto the stack.
     * @param instr The self instruction.
     * @param ctx The VM context.
     * @return Success or error on stack push failure.
     */
    [[nodiscard]]
    auto handleSelf(const Self& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Exit the current process.
     * @param instr The exit process instruction.
     * @param ctx The VM context.
     * @return Always succeeds.
     */
    [[nodiscard]]
    auto handleExitProcess(const ExitProcess& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Perform a static cast on a value.
     * @param instr The static cast instruction.
     * @param ctx The VM context.
     * @return Success or error on invalid cast.
     */
    [[nodiscard]]
    auto handleStaticCast(const StaticCast& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

    /**
     * @brief Perform a reinterpret cast on a value.
     * @param instr The reinterpret cast instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleReinterpretCast(const ReinterpretCast& instr,
        VMContext& ctx) -> std::expected<void, VMError>;

    /**
     * @brief Perform a const cast (no-op in VM context).
     * @param instr The const cast instruction.
     * @param ctx The VM context.
     * @return Success or error on stack underflow.
     */
    [[nodiscard]]
    auto handleConstCast(const ConstCast& instr, VMContext& ctx)
        -> std::expected<void, VMError>;

}
