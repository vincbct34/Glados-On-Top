/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** IStack - Interface for stack operations
*/

#pragma once

#include <expected>
#include <optional>
#include "core/RuntimeValue.hpp"

namespace rat::common
{
    /**
     * @interface IStack
     * @brief Interface for stack data structure
     */
    template <typename T>
    class IStack {
    public:
        virtual ~IStack(void)= default;

        /**
         * @brief Push an element onto the stack
         */
        virtual std::expected<void, std::string> push(T value) = 0;

        /**
         * @brief Pop an element from the stack
         */
        virtual std::expected<T, std::string> pop(void)= 0;

        /**
         * @brief Peek at the top element
         */
        virtual std::optional<T> peek(void) const= 0;

        /**
         * @brief Check if stack is empty
         */
        virtual bool empty(void) const noexcept = 0;

        /**
         * @brief Get stack size
         */
        virtual size_t size(void) const noexcept = 0;

        /**
         * @brief Clear the stack
         */
        virtual void clear(void)  noexcept = 0;
    };

} ::common
