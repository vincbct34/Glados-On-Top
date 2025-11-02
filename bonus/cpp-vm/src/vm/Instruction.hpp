/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Instruction - Type-safe instruction variant for the VM
*/

#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <variant>
#include <vector>

namespace rat
{
    /**
     * @class Instruction
     * @brief Type-safe instruction variant encapsulating all VM bytecode
     * instructions
     *
     * Instruction uses a discriminated union (std::variant) to provide type-safe,
     * pattern-matchable bytecode instruction handling. Instruction types are nested
     * within the class for API clarity and namespace management.
     *
     * Design principles:
     * - No default construction; instructions must be explicitly typed
     * - Move-friendly with minimal overhead
     * - Aligned for cache efficiency
     * - Clear nested structs for each instruction type
     */
    class alignas(64) Instruction {
        public:

            // Stack operations
            struct PushInt {
                int64_t value;
            };
            struct PushFloat {
                double value;
            };
            struct PushString {
                std::string value;
            };
            struct PushAtom {
                std::string value;
            };
            struct PushTuple {
                int size;
            };
            struct PushArray {
                int size;
            };
            struct PushUnit {};
            struct PopN {
                int count;
            };
            struct Dup {};

            // Variable operations
            struct LoadVar {
                std::string name;
            };
            struct StoreVar {
                std::string name;
            };
            struct LoadLocal {
                std::string name;
            };
            struct StoreLocal {
                std::string name;
            };

            // Array operations
            struct Index {};
            struct ArrayLength {};

            // State operations
            struct InitState {
                std::string name;
            };
            struct GetState {
                std::string name;
            };
            struct SetState {
                std::string name;
            };

            // Arithmetic operations
            struct Add {};
            struct Sub {};
            struct Mul {};
            struct Div {};
            struct Concat {};

            // Increment/decrement operations
            struct IncVar {
                std::string name;
            };
            struct DecVar {
                std::string name;
            };
            struct IncVarPost {
                std::string name;
            };
            struct DecVarPost {
                std::string name;
            };

            // Comparison operations
            struct CmpEq {};
            struct CmpNeq {};
            struct CmpLt {};
            struct CmpLe {};
            struct CmpGt {};
            struct CmpGe {};

            // Logical operations
            struct LogicAnd {};
            struct LogicOr {};
            struct LogicNot {};
            struct Negate {};

            // Maybe/Option operations
            struct PushNone {};
            struct PushBool {
                bool value;
            };
            struct GetField {
                std::string field;
            };
            struct PushJust {};
            struct PushLeft {};
            struct PushRight {};
            struct MaybeBind {
                std::string var;
            };
            struct EitherBind {
                std::string var;
            };

            // Process operations
            struct DefineProcess {
                std::string name;
            };
            struct CreateInstance {
                std::string name;
            };
            struct Send {};
            struct WaitMessage {};
            struct ProcessLoop {};
            struct Self {};
            struct ExitProcess {};

            // Function operations
            struct DefineFunction {
                std::string name;
            };
            struct CallFunction {
                std::string name;
            };

            // Pattern matching operations
            struct MatchAtom {
                std::string atom;
            };
            struct MatchVar {
                std::string var;
            };
            struct MatchTuple {
                int size;
            };
            struct MatchWildcard {};
            struct MatchInt {
                int64_t value;
            };
            struct MatchBool {
                bool value;
            };
            struct MatchString {
                std::string value;
            };

            // Type casting operations
            struct StaticCast {
                std::string type;
            };
            struct ReinterpretCast {
                std::string type;
            };
            struct ConstCast {
                std::string type;
            };

            // Control flow operations
            struct Jump {
                int offset;
            };
            struct JumpIfFalse {
                int offset;
            };
            struct JumpIfTrue {
                int offset;
            };
            struct Label {
                std::string name;
            };
            struct Call {
                std::string name;
            };
            struct Return {};

            struct Print {};
            struct Halt {};

            // Type alias for the variant type - allows external access to variant type info
            // without exposing the private _data member
            using VariantType = std::variant<PushInt,
                         PushFloat,
                         PushString,
                         PushAtom,
                         PushTuple,
                         PushArray,
                         PushUnit,
                         PopN,
                         Dup,
                         LoadVar,
                         StoreVar,
                         LoadLocal,
                         StoreLocal,
                         Index,
                         ArrayLength,
                         InitState,
                         GetState,
                         SetState,
                         Add,
                         Sub,
                         Mul,
                         Div,
                         Concat,
                         IncVar,
                         DecVar,
                         IncVarPost,
                         DecVarPost,
                         CmpEq,
                         CmpNeq,
                         CmpLt,
                         CmpLe,
                         CmpGt,
                         CmpGe,
                         LogicAnd,
                         LogicOr,
                         LogicNot,
                         Negate,
                         PushNone,
                         PushBool,
                         GetField,
                         PushJust,
                         PushLeft,
                         PushRight,
                         MaybeBind,
                         EitherBind,
                         DefineProcess,
                         CreateInstance,
                         Send,
                         WaitMessage,
                         DefineFunction,
                         CallFunction,
                         MatchAtom,
                         MatchVar,
                         MatchTuple,
                         MatchWildcard,
                         MatchInt,
                         MatchBool,
                         MatchString,
                         ProcessLoop,
                         Self,
                         ExitProcess,
                         StaticCast,
                         ReinterpretCast,
                         ConstCast,
                         Jump,
                         JumpIfFalse,
                         JumpIfTrue,
                         Label,
                         Call,
                         Return,
                         Print,
                         Halt>;

            /**
             * @brief Construct Instruction from a specific instruction subtype
             * @tparam TInstructionType One of the nested instruction struct types
             * @param instr Instruction instance
             */
            template<typename TInstructionType>
            Instruction(const TInstructionType &instr);

            Instruction(const Instruction &) = default;
            Instruction(Instruction &&) noexcept = default;
            Instruction &operator=(const Instruction &) = default;
            Instruction &operator=(Instruction &&) noexcept = default;
            ~Instruction() = default;

            /**
             * @brief Check if instruction is of a specific type
             * @tparam TInstructionType Instruction type to check against
             * @return true if current instruction matches the given type
             */
            template <typename TInstructionType>
            [[nodiscard]] bool is(void) const;

            /**
             * @brief Get mutable pointer to instruction if it matches type
             * @tparam TInstructionType Instruction type to access
             * @return Pointer to instruction if type matches, nullptr otherwise
             */
            template <typename TInstructionType>
            [[nodiscard]] TInstructionType *getIf(void);

            /**
             * @brief Get const pointer to instruction if it matches type
             * @tparam TInstructionType Instruction type to access
             * @return Const pointer to instruction if type matches, nullptr otherwise
             */
            template <typename TInstructionType>
            [[nodiscard]] const TInstructionType *getIf(void) const;

            /**
             * @brief Apply a visitor function to the instruction (mutable)
             * @tparam Visitor Visitor type
             * @param visitor Function/lambda to apply
             * @return Result of visitor application
             */
            template <typename Visitor>
            decltype(auto) visit(Visitor &&visitor);

            /**
             * @brief Apply a visitor function to the instruction (const)
             * @tparam Visitor Visitor type
             * @param visitor Function/lambda to apply
             * @return Result of visitor application
             */
            template<typename Visitor>
            decltype(auto) visit(Visitor &&visitor) const;

            /**
             * @brief Get human-readable instruction description
             * @return Formatted instruction description
             */
            [[nodiscard]]
            std::string describe(void) const;

        private:
            VariantType _data;
    };

    /**
     * @struct Bytecode
     * @brief Wrapper for bytecode instructions to provide safe span access.
     * Prevents dangling spans by owning the vector and providing a const span.
     */
    struct Bytecode {
        std::vector<Instruction> instructions;

        [[nodiscard]] std::span<const Instruction> span(void) const
        {
            return instructions;
        }

        void clear(void)
        {
            instructions.clear();
        }
        void reserve(size_t size)
        {
            instructions.reserve(size);
        }
    };

    /**
     * @struct FunctionDef
     * @brief Represents a function definition in the VM.
     * Contains the function name, parameter names, and bytecode body.
     */
    struct FunctionDef {
        std::string name;
        std::vector<std::string> params;
        Bytecode body;

        FunctionDef() = default;
        FunctionDef(std::string n, std::vector<std::string> p, Bytecode b)
                : name(std::move(n))
                , params(std::move(p))
                , body(std::move(b))
        {
        }
    };

}

#include "Instruction.inl"
