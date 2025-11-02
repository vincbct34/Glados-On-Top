/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** RuntimeValue Class Implementation
*/

#include "RuntimeValue.hpp"
#include <cassert>
#include <format>
#include <numeric>
#include <ranges>
#include <string>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace rat
{
    RuntimeValue::RuntimeValue(std::int8_t val) : _type(Type::I8),
                                                  _data(val)
    {
    }
    RuntimeValue::RuntimeValue(std::int16_t val) : _type(Type::I16),
                                                   _data(val)
    {
    }
    RuntimeValue::RuntimeValue(std::int32_t val) : _type(Type::I32),
                                                   _data(val)
    {
    }
    RuntimeValue::RuntimeValue(std::int64_t val) : _type(Type::I64),
                                                   _data(val)
    {
    }
    RuntimeValue::RuntimeValue(std::uint8_t val) : _type(Type::U8),
                                                   _data(val)
    {
    }
    RuntimeValue::RuntimeValue(std::uint16_t val) : _type(Type::U16),
                                                    _data(val)
    {
    }
    RuntimeValue::RuntimeValue(std::uint32_t val) : _type(Type::U32),
                                                    _data(val)
    {
    }
    RuntimeValue::RuntimeValue(std::uint64_t val) : _type(Type::U64),
                                                    _data(val)
    {
    }
    RuntimeValue::RuntimeValue(float val) : _type(Type::F32),
                                            _data(val)
    {
    }
    RuntimeValue::RuntimeValue(double val) : _type(Type::F64),
                                             _data(val)
    {
    }
    RuntimeValue::RuntimeValue(bool val) : _type(Type::Bool), _data(val)
    {
    }
    RuntimeValue::RuntimeValue(const std::string& val, Type t) : _type(t),
                                                                 _data(val)
    {
        assert((t == Type::String || t == Type::Atom) && "Invalid type for string");
    }
    RuntimeValue::RuntimeValue(const std::vector<RuntimeValue>& val, Type t)
                              : _type(t), _data(val) {
        assert((t == Type::Tuple || t == Type::Array) && "Invalid type for vector");
    }
    RuntimeValue::RuntimeValue(const process::ProcessIdentity& pid) : _type(Type::Pid), _data(pid.value())
    {
    }
    RuntimeValue::RuntimeValue(Type t) : _type(t),
                                         _data(std::monostate{}) {
        assert((t == Type::Unit || t == Type::None) && "Invalid type for monostate");
    }
    RuntimeValue::RuntimeValue(std::unique_ptr<RuntimeValue> val, Type t)
                              : _type(t), _data(std::move(val))
    {
        assert((t == Type::Just || t == Type::Left || t == Type::Right) && "Invalid type for unique_ptr");
    }

    RuntimeValue::RuntimeValue(const RuntimeValue& other)
        : _type(other._type), _data(std::monostate{})
    {
        copyDataFrom(other);
    }

    RuntimeValue& RuntimeValue::operator=(const RuntimeValue& other)
    {
        if (this != &other) {
            _type = other._type;
            copyDataFrom(other);
        }
        return *this;
    }

    void RuntimeValue::copyDataFrom(const RuntimeValue& other)
    {
        switch (_type) {
            case Type::I8:
                _data.emplace<std::int8_t>(std::get<std::int8_t>(other._data));
                break;
            case Type::I16:
                _data.emplace<std::int16_t>(std::get<std::int16_t>(other._data));
                break;
            case Type::I32:
                _data.emplace<std::int32_t>(std::get<std::int32_t>(other._data));
                break;
            case Type::I64:
            case Type::Pid:
                _data.emplace<std::int64_t>(std::get<std::int64_t>(other._data));
                break;
            case Type::U8:
                _data.emplace<std::uint8_t>(std::get<std::uint8_t>(other._data));
                break;
            case Type::U16:
                _data.emplace<std::uint16_t>(std::get<std::uint16_t>(other._data));
                break;
            case Type::U32:
                _data.emplace<std::uint32_t>(std::get<std::uint32_t>(other._data));
                break;
            case Type::U64:
                _data.emplace<std::uint64_t>(std::get<std::uint64_t>(other._data));
                break;
            case Type::F32:
                _data.emplace<float>(std::get<float>(other._data));
                break;
            case Type::F64:
                _data.emplace<double>(std::get<double>(other._data));
                break;
            case Type::Bool:
                _data.emplace<bool>(std::get<bool>(other._data));
                break;
            case Type::String:
            case Type::Atom:
                _data.emplace<std::string>(std::get<std::string>(other._data));
                break;
            case Type::Tuple:
            case Type::Array:
                _data.emplace<std::vector<RuntimeValue>>(std::get<std::vector<RuntimeValue>>(other._data));
                break;
            case Type::Unit:
            case Type::None:
                _data.emplace<std::monostate>();
                break;
            case Type::Just:
            case Type::Left:
            case Type::Right: {
                const auto& ptr = std::get<std::unique_ptr<RuntimeValue>>(other._data);
                if (ptr) {
                    _data.emplace<std::unique_ptr<RuntimeValue>>(std::make_unique<RuntimeValue>(*ptr));
                } else {
                    _data.emplace<std::unique_ptr<RuntimeValue>>(nullptr);
                }
                break;
            }
        }
    }

    RuntimeValue::Type RuntimeValue::type(void) const noexcept {
        return this->_type;
    }

    std::string RuntimeValue::toString(void) const {
        switch (this->_type) {
            case Type::I8:
                return std::to_string(static_cast<int>(this->getIf<std::int8_t>().value().get()));
            case Type::I16:
                return std::to_string(this->getIf<std::int16_t>().value().get());
            case Type::I32:
                return std::to_string(this->getIf<std::int32_t>().value().get());
            case Type::I64:
                return std::to_string(this->getIf<std::int64_t>().value().get());
            case Type::U8:
                return std::to_string(static_cast<unsigned>(this->getIf<std::uint8_t>().value().get()));
            case Type::U16:
                return std::to_string(this->getIf<std::uint16_t>().value().get());
            case Type::U32:
                return std::to_string(this->getIf<std::uint32_t>().value().get());
            case Type::U64:
                return std::to_string(this->getIf<std::uint64_t>().value().get());
            case Type::F32:
                return std::to_string(this->getIf<float>().value().get());
            case Type::F64:
                return std::to_string(this->getIf<double>().value().get());
            case Type::Bool:
                return std::format("{}", this->getIf<bool>().value().get());
            case Type::String:
                return std::format("\"{}\"", this->getIf<std::string>().value().get());
            case Type::Atom:
                return std::format(":{}", this->getIf<std::string>().value().get());
            case Type::Tuple: {
                auto vecOpt = this->getIf<std::vector<RuntimeValue>>();
                if (!vecOpt) return "()";
                const auto& vec = vecOpt.value().get();
                auto view = vec | std::ranges::views::transform(
                    [](const RuntimeValue& v) { return v.toString(); });
                std::string joined = std::accumulate(view.begin(), view.end(),
                    std::string{}, [](std::string acc, const std::string& s) {
                        return acc.empty()? s : std::move(acc) + ", " + s;
                    });
                return std::format("({})", joined);
            }
            case Type::Array: {
                auto vecOpt = this->getIf<std::vector<RuntimeValue>>();
                if (!vecOpt) return "[]";
                const auto& vec = vecOpt.value().get();
                auto view = vec | std::ranges::views::transform(
                    [](const RuntimeValue &v) { return v.toString(); });
                std::string joined = std::accumulate(view.begin(), view.end(),
                    std::string{}, [](std::string acc, const std::string& s) {
                        return acc.empty()? s : std::move(acc) + ", " + s;
                    });
                return std::format("[{}]", joined);
            }
            case Type::Pid: return std::format("Pid({})", this->getIf<std::int64_t>().value().get());
            case Type::Unit: return "()";
            case Type::None: return "None";
            case Type::Just: return std::format("Just({})", this->getIf<std::unique_ptr<RuntimeValue>>().value().get()->toString());
            case Type::Left: return std::format("Left({})", this->getIf<std::unique_ptr<RuntimeValue>>().value().get()->toString());
            case Type::Right: return std::format("Right({})", this->getIf<std::unique_ptr<RuntimeValue>>().value().get()->toString());
            default: return "Unknown";
        }
    }
}
