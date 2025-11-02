/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** VMError - Type-safe error variant for VM operations
*/

#pragma once

#include <variant>
#include <string>
#include <cstddef>
#include <cstdint>

namespace rat
{

    /**
     * @class VMError
     * @brief Type-safe error variant encapsulating all VM error types
     *
     * VMError uses a discriminated union (std::variant) to provide type-safe,
     * pattern-matchable error handling. Error types are nested within the class
     * for API clarity and namespace management.
     *
     * Design principles:
     * - No default construction; errors must be explicitly typed
     * - Move-friendly with minimal overhead
     * - Header-only with implementation in .inl for clarity
     * - std::format for human-readable error messages
     */
    class VMError {
        public:

            // Stack errors
            struct StackUnderflow
            {
            };
            struct StackOverflow
            {
                std::size_t maxSize;
            };

            // Type errors
            struct TypeMismatch
            {
                std::string expected;
                std::string actual;
            };
            struct InvalidCast
            {
                std::string fromType;
                std::string toType;
            };

            // Arithmetic errors
            struct DivisionByZero
            {
            };

            // Bytecode errors
            struct InvalidOpcode
            {
                std::uint8_t opcode;
            };
            struct InvalidOperand
            {
                std::string reason;
            };
            struct OutOfBounds
            {
                std::size_t index;
                std::size_t size;
            };

            // Variable errors
            struct VariableNotFound
            {
                std::string name;
            };

            // Function errors
            struct FunctionNotFound
            {
                std::string name;
            };
            struct InvalidArity
            {
                std::string functionName;
                std::size_t expected;
                std::size_t actual;
            };

            // Process errors
            struct ProcessNotFound
            {
                std::int64_t processId;
            };
            struct MailboxEmpty
            {
                std::int64_t processId;
            };

            // Pattern matching errors
            struct PatternMismatch
            {
                std::string pattern;
            };

            // State errors
            struct InvalidState
            {
                std::string reason;
            };
            struct NullPointer
            {
                std::string context;
            };

            // ================================================================
            // Constructor
            // ================================================================

            /**
             * @brief Construct VMError from a specific error subtype
             * @tparam TErrorSubtype One of the nested error struct types
             * @param error Error instance
             */
            template <typename TErrorSubtype>
            VMError(const TErrorSubtype &error);

            // ================================================================
            // Type-checking and access methods
            // ================================================================

            /**
             * @brief Check if error is of a specific type
             * @tparam TErrorSubtype Error type to check against
             * @return true if current error matches the given type
             */
            template <typename TErrorSubtype>
            [[nodiscard]]
            bool is(void) const;

            /**
             * @brief Get mutable pointer to error if it matches type
             * @tparam TErrorSubtype Error type to access
             * @return Pointer to error if type matches, nullptr otherwise
             */
            template <typename TErrorSubtype>
            [[nodiscard]]
            TErrorSubtype *getIf(void);

            /**
             * @brief Get const pointer to error if it matches type
             * @tparam TErrorSubtype Error type to access
             * @return Const pointer to error if type matches, nullptr otherwise
             */
            template <typename TErrorSubtype>
            [[nodiscard]]
            const TErrorSubtype *getIf(void) const;

            // ================================================================
            // Visitor pattern
            // ================================================================

            /**
             * @brief Apply a visitor function to the error (mutable)
             * @tparam Visitor Visitor type
             * @param visitor Function/lambda to apply
             * @return Result of visitor application
             */
            template <typename Visitor>
            decltype(auto) visit(Visitor &&visitor);

            /**
             * @brief Apply a visitor function to the error (const)
             * @tparam Visitor Visitor type
             * @param visitor Function/lambda to apply
             * @return Result of visitor application
             */
            template <typename Visitor>
            decltype(auto) visit(Visitor &&visitor) const;

            /**
             * @brief Get human-readable error message
             * @return Formatted error description
             */
            [[nodiscard]]
            std::string describe(void) const;

        private:
            std::variant<StackUnderflow,
                         StackOverflow,
                         TypeMismatch,
                         InvalidCast,
                         DivisionByZero,
                         InvalidOpcode,
                         InvalidOperand,
                         OutOfBounds,
                         VariableNotFound,
                         FunctionNotFound,
                         InvalidArity,
                         ProcessNotFound,
                         MailboxEmpty,
                         PatternMismatch,
                         InvalidState,
                         NullPointer> _value;

            template <typename T, typename... Ts>
            [[nodiscard]]
            static constexpr bool isInParameterPack(const std::variant<Ts...> *)
            {
                return std::disjunction_v<std::is_same<T, Ts>...>;
            }

            template <typename T>
            static constexpr bool isErrorSubtype = isInParameterPack<T>(decltype (&_value)(nullptr));

            friend class WindowBase;

            template <typename Handler, typename... Ts>
            [[nodiscard]]
            static constexpr bool isInvocableWithErrorSubtype(const std::variant<Ts...> *)
            {
                return std::disjunction_v<std::is_invocable<Handler &, Ts &>...>;
            }

            template <typename Handler>
            static constexpr bool isErrorHandler = isInvocableWithErrorSubtype<Handler>(decltype (&_value)(nullptr));
    };

}

#include "ErrorCode.inl"
