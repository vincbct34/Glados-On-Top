/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** VariableStore Class Implementation
*/

#include <format>
#include "VariableStore.hpp"

namespace rat
{
    auto VariableStore::get(const std::string &name) const
        -> std::expected<RuntimeValue, VMError>
    {
        auto it = this->_store.find(name);
        if (it == this->_store.end())
            return std::unexpected(VMError::VariableNotFound{name});
        return it->second;
    }

    auto VariableStore::set(const std::string &name, RuntimeValue value)
        -> std::expected<void, VMError>
    {
        this->_store.insert_or_assign(name, std::move(value));
        return {};
    }

    void VariableStore::clear(void)  noexcept
    {
        this->_store.clear();
    }
}
