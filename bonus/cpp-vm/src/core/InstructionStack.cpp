/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** InstructionStack Class Implementation
*/

#include "InstructionStack.hpp"

namespace rat
{
    InstructionStack::InstructionStack(size_t maxSize) : _maxSize(maxSize) {}

    auto InstructionStack::push(const RuntimeValue& val)
        -> std::expected<void, VMError>
    {
        if (this->_stack.size()>= this->_maxSize)
            return std::unexpected(VMError::StackOverflow{this->_maxSize});
        this->_stack.push_back(val);
        return {};
    }

    auto InstructionStack::pop(void)
        -> std::expected<RuntimeValue, VMError>
    {
        if (this->_stack.empty())
            return std::unexpected(VMError::StackUnderflow{});
        RuntimeValue val = std::move(this->_stack.back());
        this->_stack.pop_back();
        return val;
    }

    auto InstructionStack::peek(size_t offset) const
        -> std::expected<std::reference_wrapper<const RuntimeValue>, VMError>
    {
        if (offset >= this->_stack.size())
            return std::unexpected(VMError::OutOfBounds{offset, this->_stack.size()});
        return std::ref(this->_stack[this->_stack.size()- 1 - offset]);
    }

    size_t InstructionStack::size(void) const noexcept
    {
        return this->_stack.size();
    }

    void InstructionStack::clear(void) noexcept
    {
        this->_stack.clear();
    }
}
