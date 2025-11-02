/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Instruction Dispatcher Implementation (inline)
*/

#pragma once

#include <type_traits>

namespace rat
{
    /**
     * @brief Helper to compute the index of a type in a variant
     * @tparam T Type to find
     * @tparam Variant Variant type to search in
     */
    template<typename T, typename Variant>
    struct variant_index;

    /**
     * @brief Specialization that computes variant index using fold expression
     * @tparam T Type to find
     * @tparam Types Variant alternative types
     */
    template<typename T, typename... Types>
    struct variant_index<T, std::variant<Types...>>
        : std::integral_constant<
            size_t,
            []<size_t... Is>(std::index_sequence<Is...>)
            {
                size_t result = 0;
                ((std::is_same_v<T, Types> ? (result = Is, true) : false)
                    || ...);
                return result;
            } (std::index_sequence_for<Types...> {})>
    {
    };

    /**
     * @brief Register a typed handler for a specific instruction type
     * @tparam InstrType The instruction type to handle
     * @param handler Function pointer to the handler
     */
    template<typename InstrType>
    inline void InstructionDispatcher::registerHandler(
        TypedHandlerFn<InstrType> handler)
    {
        constexpr size_t index = variant_index<
            InstrType,
            Instruction::VariantType>::value;
        this->_handlers[index] = reinterpret_cast<void *>(handler);
    }

    /**
     * @brief Get the registered handler for a specific instruction type
     * @tparam InstrType The instruction type
     * @return TypedHandlerFn<InstrType> Handler function pointer
     */
    template<typename InstrType>
    inline auto InstructionDispatcher::getHandler(void) const
        -> TypedHandlerFn<InstrType>
    {
        constexpr size_t index = variant_index<
            InstrType,
            Instruction::VariantType>::value;
        return reinterpret_cast<TypedHandlerFn<InstrType>>(
            this->_handlers[index]);
    }

    /**
     * @brief Dispatch an instruction to its registered handler
     * @param instr Instruction to dispatch
     * @param ctx VM execution context
     * @return std::expected<void, VMError> Success or error
     */
    inline auto InstructionDispatcher::dispatch(const Instruction &instr,
                                                VMContext &ctx)
        -> std::expected<void, VMError>
    {
        DispatchVisitor visitor{ctx, *this};
        return instr.visit(visitor);
    }
}
