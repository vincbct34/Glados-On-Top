/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** InstructionStack Template Implementations
*/

#include "InstructionStack.hpp"

namespace rat
{
    template<typename Func>
    void InstructionStack::forEach(Func &&func) const
    {
        std::ranges::for_each(this->_stack | std::views::reverse,
                              std::forward<Func>(func));
    }
}
