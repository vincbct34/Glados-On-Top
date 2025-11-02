/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** ProcessIdentity - Strong typedef for process IDs
*/

#pragma once

#include <cstdint>
#include <compare>

namespace rat::process
{
    /**
     * @class ProcessIdentity
     * @brief Strong typedef for process IDs to prevent mixing with other integers
     * Provides type safety and comparison operators
     */
    class ProcessIdentity {
    public:
        ProcessIdentity(void)= default;
        explicit ProcessIdentity(std::int64_t id) noexcept : _id(id) {}

        [[nodiscard]] std::int64_t value(void) const noexcept { return _id; }

        // Comparison operators
        auto operator<=>(const ProcessIdentity&) const = default;

        // Arithmetic (for PID allocation)
        ProcessIdentity& operator++() noexcept { ++_id; return *this; }
        ProcessIdentity operator++(int) noexcept { ProcessIdentity temp = *this; ++_id; return temp; }

        // Conversion to underlying type
        explicit operator std::int64_t(void) const noexcept { return _id; }

    private:
        std::int64_t _id = 0;
    };

    // User-defined literals for convenience
    inline ProcessIdentity operator""_pid(unsigned long long id) noexcept {
        return ProcessIdentity(static_cast<std::int64_t>(id));
    }

} // namespace rat::process
