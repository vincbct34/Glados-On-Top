/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** RuntimeValue - Type-safe runtime value variant for the VM
*/

#pragma once

#include <cstdint>
#include <variant>
#include <vector>
#include <string>
#include <memory>
#include "process/ProcessIdentity.hpp"

namespace rat
{
    /**
     * @class RuntimeValue
     * @brief Type-safe runtime value variant encapsulating all VM value types
     *
     * RuntimeValue uses a discriminated union (std::variant) to provide type-safe,
     * pattern-matchable runtime value handling. Value types are nested within the class
     * for API clarity and namespace management.
     *
     * Design principles:
     * - No default construction; values must be explicitly typed
     * - Move-friendly with minimal overhead for large values
     * - Supports deep copy for recursive types (Just/Left/Right)
     * - std::format for human-readable value representation
     */
    class alignas(64) RuntimeValue
    {
    public:
        // ================================================================
        // Value type definitions (nested structs for API clarity)
        // ================================================================

        // Primitive signed integers
        struct I8 {
            std::int8_t value;
        };
        struct I16 {
            std::int16_t value;
        };
        struct I32 {
            std::int32_t value;
        };
        struct I64 {
            std::int64_t value;
        };

        // Primitive unsigned integers
        struct U8 {
            std::uint8_t value;
        };
        struct U16 {
            std::uint16_t value;
        };
        struct U32 {
            std::uint32_t value;
        };
        struct U64 {
            std::uint64_t value;
        };

        // Floating point
        struct F32 {
            float value;
        };
        struct F64 {
            double value;
        };

        // Boolean
        struct Bool {
            bool value;
        };

        // Text types
        struct String {
            std::string value;
        };
        struct Atom {
            std::string value;
        };

        // Collection types
        struct Tuple {
            std::vector<RuntimeValue> elements;
        };
        struct Array {
            std::vector<RuntimeValue> elements;
        };

        // Process identifier
        struct Pid {
            std::int64_t value;
        };

        // Unit type (void/empty)
        struct Unit { };

        // Maybe/Option type
        struct None { };
        struct Just {
            std::unique_ptr<RuntimeValue> value;
        };

        // Either type
        struct Left {
            std::unique_ptr<RuntimeValue> value;
        };
        struct Right {
            std::unique_ptr<RuntimeValue> value;
        };


        // Primitive constructors (implicit conversion for convenience)
        RuntimeValue(std::int8_t val) : _data(I8{val}) {}
        RuntimeValue(std::int16_t val) : _data(I16{val}) {}
        RuntimeValue(std::int32_t val) : _data(I32{val}) {}
        RuntimeValue(std::int64_t val) : _data(I64{val}) {}
        RuntimeValue(std::uint8_t val) : _data(U8{val}) {}
        RuntimeValue(std::uint16_t val) : _data(U16{val}) {}
        RuntimeValue(std::uint32_t val) : _data(U32{val}) {}
        RuntimeValue(std::uint64_t val) : _data(U64{val}) {}
        RuntimeValue(float val) : _data(F32{val}) {}
        RuntimeValue(double val) : _data(F64{val}) {}
        RuntimeValue(bool val) : _data(Bool{val}) {}

        // String constructor (for convenience)
        RuntimeValue(const std::string& val) : _data(String{val}) {}
        RuntimeValue(std::string&& val) : _data(String{std::move(val)}) {}

        // Explicit nested struct constructors
        template <typename TValueType>
        RuntimeValue(const TValueType& value);

        // Copy constructor with deep copy support
        RuntimeValue(const RuntimeValue& other);
        RuntimeValue(RuntimeValue&& other) noexcept = default;

        // Copy assignment with deep copy support
        RuntimeValue& operator=(const RuntimeValue& other);
        RuntimeValue& operator=(RuntimeValue&& other) noexcept = default;

        ~RuntimeValue(void)= default;

        // ================================================================
        // Type-checking and access methods
        // ================================================================

        /**
         * @brief Check if value is of a specific type
         * @tparam TValueType Value type to check against
         * @return true if current value matches the given type
         */
        template <typename TValueType>
        [[nodiscard]] bool is(void) const;

        /**
         * @brief Get mutable pointer to value if it matches type
         * @tparam TValueType Value type to access
         * @return Pointer to value if type matches, nullptr otherwise
         */
        template <typename TValueType>
        [[nodiscard]] TValueType* getIf();

        /**
         * @brief Get const pointer to value if it matches type
         * @tparam TValueType Value type to access
         * @return Const pointer to value if type matches, nullptr otherwise
         */
        template <typename TValueType>
        [[nodiscard]] const TValueType* getIf() const;

        // ================================================================
        // Visitor pattern
        // ================================================================

        /**
         * @brief Apply a visitor function to the value (mutable)
         * @tparam Visitor Visitor type
         * @param visitor Function/lambda to apply
         * @return Result of visitor application
         */
        template <typename Visitor>
        decltype(auto) visit(Visitor&& visitor);

        /**
         * @brief Apply a visitor function to the value (const)
         * @tparam Visitor Visitor type
         * @param visitor Function/lambda to apply
         * @return Result of visitor application
         */
        template <typename Visitor>
        decltype(auto) visit(Visitor&& visitor) const;

        /**
         * @brief Get human-readable string representation
         * @return Formatted value description
         */
        [[nodiscard]]
        std::string toString(void) const;

    private:
        std::variant<I8, I16, I32, I64,
                     U8, U16, U32, U64,
                     F32, F64,
                     Bool,
                     String,
                     Atom,
                     Tuple,
                     Array,
                     Pid,
                     Unit,
                     None, Just,
                     Left, Right> _data;

        void copyDataFrom(const RuntimeValue &other);
    };

}

#include "RuntimeValue_New.inl"
