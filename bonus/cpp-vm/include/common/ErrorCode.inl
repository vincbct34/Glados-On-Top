/**
 * @file ErrorCode.inl
 * @brief Implementation of VMError template methods and describe() function
 *
 * This file contains all template implementations and the describe() method
 * for the VMError class. It is included by ErrorCode.hpp and must not be
 * included directly.
 */

#pragma once

#include <format>
#include <type_traits>
#include <utility>

namespace rat
{

    // ================================================================
    // Constructor: Initialize from error subtype
    // ================================================================

    template <typename TErrorSubtype>
    VMError::VMError(const TErrorSubtype &error)
        : _value(error)
    {
        static_assert(isErrorSubtype<TErrorSubtype>,
                      "TErrorSubtype must be a valid VMError subtype");
    }

    // ================================================================
    // Type checking: Is current error of given type?
    // ================================================================

    template <typename TErrorSubtype>
    bool VMError::is() const
    {
        static_assert(isErrorSubtype<TErrorSubtype>,
                      "TErrorSubtype must be a valid VMError subtype");
        return std::holds_alternative<TErrorSubtype>(this->_value);
    }

    // ================================================================
    // Safe access: Get mutable pointer to error subtype
    // ================================================================

    template <typename TErrorSubtype>
    TErrorSubtype *VMError::getIf()
    {
        static_assert(isErrorSubtype<TErrorSubtype>,
                      "TErrorSubtype must be a valid VMError subtype");
        return std::get_if<TErrorSubtype>(&this->_value);
    }

    // ================================================================
    // Safe access: Get const pointer to error subtype
    // ================================================================

    template <typename TErrorSubtype>
    const TErrorSubtype *VMError::getIf() const
    {
        static_assert(isErrorSubtype<TErrorSubtype>,
                      "TErrorSubtype must be a valid VMError subtype");
        return std::get_if<TErrorSubtype>(&this->_value);
    }

    // ================================================================
    // Visitor pattern: Apply visitor to error (mutable)
    // ================================================================

    template <typename Visitor>
    decltype(auto) VMError::visit(Visitor &&visitor)
    {
        static_assert(isErrorHandler<Visitor>(),
                      "Visitor must be invocable with at least one VMError subtype");
        return std::visit(std::forward<Visitor>(visitor), this->_value);
    }

    // ================================================================
    // Visitor pattern: Apply visitor to error (const)
    // ================================================================

    template <typename Visitor>
    decltype(auto) VMError::visit(Visitor &&visitor) const
    {
        static_assert(isErrorHandler<Visitor>(),
                      "Visitor must be invocable with at least one VMError subtype");
        return std::visit(std::forward<Visitor>(visitor), this->_value);
    }

    // ================================================================
    // Error formatting: Human-readable error message
    // ================================================================

    inline std::string VMError::describe(void) const
    {
        return std::visit([](const auto &err) -> std::string {
            using T = std::decay_t<decltype(err)>;

            if constexpr (std::is_same_v<T, StackUnderflow>) {
                return "Stack underflow";
            }
            else if constexpr (std::is_same_v<T, StackOverflow>) {
                return std::format("Stack overflow (max size: {})",
                                   err.maxSize);
            }
            else if constexpr (std::is_same_v<T, TypeMismatch>) {
                return std::format("Type mismatch: expected {}, got {}", 
                                   err.expected, err.actual);
            }
            else if constexpr (std::is_same_v<T, InvalidCast>) {
                return std::format("Invalid cast from {} to {}", 
                                   err.fromType, err.toType);
            }
            else if constexpr (std::is_same_v<T, DivisionByZero>) {
                return "Division by zero";
            }
            else if constexpr (std::is_same_v<T, InvalidOpcode>) {
                return std::format("Invalid opcode: 0x{0:02x}", 
                                   static_cast<unsigned>(err.opcode));
            }
            else if constexpr (std::is_same_v<T, InvalidOperand>) {
                return std::format("Invalid operand: {}", err.reason);
            }
            else if constexpr (std::is_same_v<T, OutOfBounds>) {
                return std::format("Index {} out of bounds (size: {})", 
                                   err.index, err.size);
            }
            else if constexpr (std::is_same_v<T, VariableNotFound>) {
                return std::format("Variable not found: {}", err.name);
            }
            else if constexpr (std::is_same_v<T, FunctionNotFound>) {
                return std::format("Function not found: {}", err.name);
            }
            else if constexpr (std::is_same_v<T, InvalidArity>) {
                return std::format("Function {} expects {} arguments, got {}", 
                                   err.functionName, err.expected, err.actual);
            }
            else if constexpr (std::is_same_v<T, ProcessNotFound>) {
                return std::format("Process not found: {}", err.processId);
            }
            else if constexpr (std::is_same_v<T, MailboxEmpty>) {
                return std::format("Mailbox empty for process {}", err.processId);
            }
            else if constexpr (std::is_same_v<T, PatternMismatch>) {
                return std::format("Pattern mismatch: {}", err.pattern);
            }
            else if constexpr (std::is_same_v<T, InvalidState>) {
                return std::format("Invalid state: {}", err.reason);
            }
            else if constexpr (std::is_same_v<T, NullPointer>) {
                return std::format("Null pointer in {}", err.context);
            }
            else {
                return "Unknown error";
            }
        }, this->_value);
    }

} 
