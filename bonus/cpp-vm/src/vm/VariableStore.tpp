/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** VariableStore Template Implementations
*/

#include "VariableStore.hpp"

namespace rat
{
    // Functional-style
    template <typename Func>
    void VariableStore::forEach(Func &&func) const
    {
        for (const auto &[k, v] : this->_store)
            func(k, v);
    }
}
