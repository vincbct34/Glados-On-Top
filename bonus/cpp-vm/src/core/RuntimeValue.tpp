/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** RuntimeValue Template Implementations
*/

#pragma once

#include "RuntimeValue.hpp"

namespace rat
{
    template<typename T>
    bool RuntimeValue::is(void) const {
        return std::holds_alternative<T>(_data);
    }

    template<typename T>
    std::optional<std::reference_wrapper<const T>> RuntimeValue::getIf() const {
        if (auto* ptr = std::get_if<T>(&_data)) {
            return std::ref(*ptr);
        }
        return std::nullopt;
    }

    template<typename Visitor>
    decltype(auto) RuntimeValue::visit(Visitor&& vis) const {
        return std::visit(std::forward<Visitor>(vis), _data);
    }
}
