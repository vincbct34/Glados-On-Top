/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** RuntimeValue Implementation (inline)
*/

#pragma once

#include <format>
#include <numeric>
#include <ranges>
#include <algorithm>
#include <cassert>

namespace rat
{
    // ================================================================
    // Explicit nested struct constructor
    // ================================================================

    template <typename TValueType>
    RuntimeValue::RuntimeValue(const TValueType& value) : _data(value)
    {
    }

    // ================================================================
    // Copy constructor with deep copy support
    // ================================================================

    inline RuntimeValue::RuntimeValue(const RuntimeValue& other)
    {
        // Manual copy for each type to handle unique_ptr correctly
        std::visit([this](const auto& value) {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<T, Just>) {
                _data = value.value ? Just{std::make_unique<RuntimeValue>(*value.value)} : Just{nullptr};
            } else if constexpr (std::is_same_v<T, Left>) {
                _data = value.value ? Left{std::make_unique<RuntimeValue>(*value.value)} : Left{nullptr};
            } else if constexpr (std::is_same_v<T, Right>) {
                _data = value.value ? Right{std::make_unique<RuntimeValue>(*value.value)} : Right{nullptr};
            } else {
                _data = value;
            }
        }, other._data);
    }

    // ================================================================
    // Copy assignment with deep copy support
    // ================================================================

    inline RuntimeValue& RuntimeValue::operator=(const RuntimeValue& other)
    {
        if (this != &other) {
            // Manual copy for each type to handle unique_ptr correctly
            std::visit([this](const auto& value) {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<T, Just>) {
                    _data = value.value ? Just{std::make_unique<RuntimeValue>(*value.value)} : Just{nullptr};
                } else if constexpr (std::is_same_v<T, Left>) {
                    _data = value.value ? Left{std::make_unique<RuntimeValue>(*value.value)} : Left{nullptr};
                } else if constexpr (std::is_same_v<T, Right>) {
                    _data = value.value ? Right{std::make_unique<RuntimeValue>(*value.value)} : Right{nullptr};
                } else {
                    _data = value;
                }
            }, other._data);
        }
        return *this;
    }

    // ================================================================
    // Type-checking and access methods
    // ================================================================

    template <typename TValueType>
    [[nodiscard]] bool RuntimeValue::is(void) const{
        return std::holds_alternative<TValueType>(_data);
    }

    template <typename TValueType>
    [[nodiscard]] TValueType* RuntimeValue::getIf(){
        return std::get_if<TValueType>(&_data);
    }

    template <typename TValueType>
    [[nodiscard]] const TValueType* RuntimeValue::getIf() const{
        return std::get_if<TValueType>(&_data);
    }

    // ================================================================
    // Visitor pattern
    // ================================================================

    template <typename Visitor>
    decltype(auto) RuntimeValue::visit(Visitor&& visitor)
    {
        return std::visit(std::forward<Visitor>(visitor), _data);
    }

    template <typename Visitor>
    decltype(auto) RuntimeValue::visit(Visitor&& visitor) const
    {
        return std::visit(std::forward<Visitor>(visitor), _data);
    }

    // ================================================================
    // String representation
    // ================================================================

    inline std::string RuntimeValue::toString(void) const{
        return visit([](const auto& value) -> std::string {
            using T = std::decay_t<decltype(value)>;

            if constexpr (std::is_same_v<T, I8>) {
                return std::to_string(static_cast<int>(value.value));
            } else if constexpr (std::is_same_v<T, I16>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, I32>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, I64>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, U8>) {
                return std::to_string(static_cast<unsigned>(value.value));
            } else if constexpr (std::is_same_v<T, U16>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, U32>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, U64>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, F32>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, F64>) {
                return std::to_string(value.value);
            } else if constexpr (std::is_same_v<T, Bool>) {
                return std::format("{}", value.value);
            } else if constexpr (std::is_same_v<T, String>) {
                return std::format("\"{}\"", value.value);
            } else if constexpr (std::is_same_v<T, Atom>) {
                return std::format(":{}", value.value);
            } else if constexpr (std::is_same_v<T, Tuple>) {
                auto view = value.elements | std::ranges::views::transform(
                    [](const RuntimeValue& v) { return v.toString(void); });
                std::string joined = std::accumulate(view.begin(), view.end(),
                    std::string{}, [](std::string acc, const std::string& s) {
                        return acc.empty()? s : std::move(acc) + ", " + s;
                    });
                return std::format("({})", joined);
            } else if constexpr (std::is_same_v<T, Array>) {
                auto view = value.elements | std::ranges::views::transform(
                    [](const RuntimeValue& v) { return v.toString(void); });
                std::string joined = std::accumulate(view.begin(), view.end(),
                    std::string{}, [](std::string acc, const std::string& s) {
                        return acc.empty()? s : std::move(acc) + ", " + s;
                    });
                return std::format("[{}]", joined);
            } else if constexpr (std::is_same_v<T, Pid>) {
                return std::format("Pid({})", value.value);
            } else if constexpr (std::is_same_v<T, Unit>) {
                return "()";
            } else if constexpr (std::is_same_v<T, None>) {
                return "None";
            } else if constexpr (std::is_same_v<T, Just>) {
                return std::format("Just({})", value.value ? value.value->toString(void): "null");
            } else if constexpr (std::is_same_v<T, Left>) {
                return std::format("Left({})", value.value ? value.value->toString(void): "null");
            } else if constexpr (std::is_same_v<T, Right>) {
                return std::format("Right({})", value.value ? value.value->toString(void): "null");
            } else {
                return "Unknown";
            }
        });
    }

} 
