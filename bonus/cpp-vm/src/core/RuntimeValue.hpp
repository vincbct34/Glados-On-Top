/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** RuntimeValue Class Definition
*/

#pragma once

#include "process/ProcessIdentity.hpp"

#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace rat
{
    /**
     * @class RuntimeValue
     * @brief Thin wrapper around std::variant for runtime values in the VM.
     * Provides templated accessors and visit for type-safe operations.
     * @author Robin Toillon
     */
    class alignas(64) RuntimeValue {
        public:
            enum class Type {
                I8,
                I16,
                I32,
                I64,
                U8,
                U16,
                U32,
                U64,
                F32,
                F64,
                Bool,
                String,
                Atom,
                Tuple,
                Array,
                Pid,
                Unit,
                None,
                Just,
                Left,
                Right
            };

            RuntimeValue(std::int8_t val);
            RuntimeValue(std::int16_t val);
            RuntimeValue(std::int32_t val);
            RuntimeValue(std::int64_t val);
            RuntimeValue(std::uint8_t val);
            RuntimeValue(std::uint16_t val);
            RuntimeValue(std::uint32_t val);
            RuntimeValue(std::uint64_t val);
            RuntimeValue(float val);
            RuntimeValue(double val);
            RuntimeValue(bool val);
            RuntimeValue(const std::string &val, Type t = Type::String);
            RuntimeValue(const std::vector<RuntimeValue> &val, Type t = Type::Tuple);
            RuntimeValue(const process::ProcessIdentity &pid);
            RuntimeValue(Type t);
            RuntimeValue(
                std::unique_ptr<RuntimeValue> val, Type t);

            RuntimeValue(const RuntimeValue &other);
            RuntimeValue(RuntimeValue &&other) noexcept = default;
            RuntimeValue &operator=(const RuntimeValue &other);
            RuntimeValue &operator=(RuntimeValue &&other) noexcept = default;

            ~RuntimeValue() = default;

            [[nodiscard]]
            Type type(void) const noexcept;

            template<typename T>
            [[nodiscard]]
            bool is(void) const;

            template<typename T>
            [[nodiscard]]
            std::optional<std::reference_wrapper<const T>> getIf(void) const;

            template<typename Visitor>
            decltype(auto) visit(Visitor &&vis) const;

            [[nodiscard]]
            std::string toString(void) const;

        private:
            Type _type;
            std::variant<std::int8_t,
                std::int16_t,
                std::int32_t,
                std::int64_t,
                std::uint8_t,
                std::uint16_t,
                std::uint32_t,
                std::uint64_t,
                float,
                double,
                bool,
                std::string,
                std::vector<RuntimeValue>,
                std::monostate,
                std::unique_ptr<RuntimeValue>>
                _data;

            void copyDataFrom(const RuntimeValue &other);
    };
}

#include "RuntimeValue.tpp"