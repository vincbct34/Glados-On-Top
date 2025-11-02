/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** PatternMatcher Implementation
*/

#include "PatternMatcher.hpp"

namespace rat
{
    bool PatternMatcher::matchAtom(const RuntimeValue &value,
                                   const std::string &expectedAtom) const
    {
        if (value.type() != RuntimeValue::Type::Atom)
            return false;
        
        auto atomVal = value.getIf<std::string>();
        return atomVal && atomVal->get()== expectedAtom;
    }

    bool PatternMatcher::matchTuple(const RuntimeValue &value,
                                    size_t expectedSize,
                                    std::vector<RuntimeValue> &elements) const
    {
        if (value.type() != RuntimeValue::Type::Tuple)
            return false;
        
        auto tupleVal = value.getIf<std::vector<RuntimeValue>>();
        if (!tupleVal || tupleVal->get().size()!= expectedSize)
            return false;
        
        elements = tupleVal->get();
        return true;
    }

    bool PatternMatcher::matchInt(const RuntimeValue &value,
                                  int64_t expected) const
    {
        if (value.type() != RuntimeValue::Type::I64)
            return false;
        
        auto intVal = value.getIf<std::int64_t>();
        return intVal && *intVal == expected;
    }

    bool PatternMatcher::matchBool(const RuntimeValue &value,
                                   bool expected) const
    {
        if (value.type() != RuntimeValue::Type::Bool)
            return false;
        
        auto boolVal = value.getIf<bool>();
        return boolVal && *boolVal == expected;
    }

    bool PatternMatcher::matchString(const RuntimeValue &value,
                                     const std::string &expected) const
    {
        if (value.type() != RuntimeValue::Type::String)
            return false;

        auto strVal = value.getIf<std::string>();
        return strVal && strVal->get()== expected;
    }

    bool PatternMatcher::matchWildcard(void) const noexcept
    {
        return true;
    }

    auto PatternMatcher::bindVariable(const std::string &varName,
                                      RuntimeValue value,
                                      VariableStore &locals) const
        -> std::expected<void, VMError>
    {
        return locals.set(varName, std::move(value));
    }
}
