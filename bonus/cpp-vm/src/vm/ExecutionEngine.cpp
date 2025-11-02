/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Optimized Execution Engine - Implementation
*/

#include "ExecutionEngine.hpp"

namespace rat
{
    ExecutionEngine::ExecutionEngine(const Bytecode &bytecode,
                                     VMContext &ctx,
                                     InstructionDispatcher &dispatcher)
                                    : _bytecode(bytecode)
                                    , _context(ctx)
                                    , _dispatcher(dispatcher)
    {
        this->reset();
    }

    auto ExecutionEngine::run(void) -> std::expected<void, VMError>
    {
        const auto &instructions = this->_bytecode.span();
        const size_t instructionCount = instructions.size();

        while (this->_context.isRunning() &&
               this->_context.programCounter() < instructionCount) [[likely]] {
            const auto &instr = instructions[this->_context.programCounter()];
            ++this->_context.programCounter();

            auto result = this->_dispatcher.dispatch(instr, _context);
            if (!result) [[unlikely]]
                return result;
        }

        if (this->_context.programCounter() >= instructionCount &&
            this->_context.isRunning())
            [[unlikely]] {
            return std::unexpected(VMError {
                VMError::InvalidState { "Execution ended without HALT" } });
        }

        return {};
    }

    auto ExecutionEngine::step(void) -> std::expected<void, VMError>
    {
        const auto &instructions = _bytecode.span();

        if (!this->_context.isRunning()) [[unlikely]]
            return std::unexpected(
                VMError::InvalidState{"Execution not running"});

        if (this->_context.programCounter() >= instructions.size()) [[unlikely]]
            return std::unexpected(
                VMError::InvalidState{"Program counter out of bounds"});

        const auto &instr = instructions[this->_context.programCounter()];
        ++this->_context.programCounter();

        return this->_dispatcher.dispatch(instr, this->_context);
    }

    bool ExecutionEngine::isRunning(void) const noexcept
    {
        return this->_context.isRunning();
    }

    void ExecutionEngine::reset(void) noexcept
    {
        this->_context.programCounter() = 0;
        this->_context.setRunning(true);
    }
}
