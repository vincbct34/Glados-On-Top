/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Test for new RuntimeValue and Instruction classes
*/

#include "core/RuntimeValue_New.hpp"
#include "vm/Instruction_New.hpp"
#include <iostream>
#include <cassert>

using namespace rat;

void testRuntimeValue()
{
    std::cout << "Testing RuntimeValue...\n";

    // Test primitive construction
    RuntimeValue i32(42);
    RuntimeValue str("hello");
    RuntimeValue boolean(true);

    // Test type checking
    assert(i32.is<RuntimeValue::I32>());
    assert(!i32.is<RuntimeValue::String>());
    assert(str.is<RuntimeValue::String>());
    assert(boolean.is<RuntimeValue::Bool>());

    // Test access
    if (auto* val = i32.getIf<RuntimeValue::I32>()) {
        assert(val->value == 42);
        std::cout << "  I32 value: " << val->value << " ✓\n";
    }

    if (auto* s = str.getIf<RuntimeValue::String>()) {
        assert(s->value == "hello");
        std::cout << "  String value: " << s->value << " ✓\n";
    }

    // Test toString
    std::cout << "  I32 toString: " << i32.toString() << " ✓\n";
    std::cout << "  String toString: " << str.toString() << " ✓\n";
    std::cout << "  Bool toString: " << boolean.toString() << " ✓\n";

    // Test tuple
    RuntimeValue tuple(RuntimeValue::Tuple{{i32, str}});
    assert(tuple.is<RuntimeValue::Tuple>());
    std::cout << "  Tuple toString: " << tuple.toString() << " ✓\n";

    // Test array
    RuntimeValue arr(RuntimeValue::Array{{i32, RuntimeValue(100)}});
    assert(arr.is<RuntimeValue::Array>());
    std::cout << "  Array toString: " << arr.toString() << " ✓\n";

    // Test Unit
    RuntimeValue unit(RuntimeValue::Unit{});
    assert(unit.is<RuntimeValue::Unit>());
    std::cout << "  Unit toString: " << unit.toString() << " ✓\n";

    // Test None
    RuntimeValue none(RuntimeValue::None{});
    assert(none.is<RuntimeValue::None>());
    std::cout << "  None toString: " << none.toString() << " ✓\n";

    // Test Just
    RuntimeValue just(RuntimeValue::Just{std::make_unique<RuntimeValue>(42)});
    assert(just.is<RuntimeValue::Just>());
    std::cout << "  Just toString: " << just.toString() << " ✓\n";

    // Test copy constructor with Just
    RuntimeValue just_copy = just;
    assert(just_copy.is<RuntimeValue::Just>());
    std::cout << "  Just copy: " << just_copy.toString() << " ✓\n";

    // Test visitor
    int visitor_result = i32.visit([](const auto& v) -> int {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, RuntimeValue::I32>) {
            return v.value;
        }
        return -1;
    });
    assert(visitor_result == 42);
    std::cout << "  Visitor result: " << visitor_result << " ✓\n";

    std::cout << "RuntimeValue tests passed! ✅\n\n";
}

void testInstruction()
{
    std::cout << "Testing Instruction...\n";

    // Test construction
    Instruction pushInt(Instruction::PushInt{42});
    Instruction pushStr(Instruction::PushString{"hello"});
    Instruction add(Instruction::Add{});

    // Test type checking
    assert(pushInt.is<Instruction::PushInt>());
    assert(!pushInt.is<Instruction::Add>());
    assert(pushStr.is<Instruction::PushString>());
    assert(add.is<Instruction::Add>());

    // Test access
    if (auto* push = pushInt.getIf<Instruction::PushInt>()) {
        assert(push->value == 42);
        std::cout << "  PushInt value: " << push->value << " ✓\n";
    }

    if (auto* push = pushStr.getIf<Instruction::PushString>()) {
        assert(push->value == "hello");
        std::cout << "  PushString value: " << push->value << " ✓\n";
    }

    // Test describe
    std::cout << "  PushInt describe: " << pushInt.describe() << " ✓\n";
    std::cout << "  PushString describe: " << pushStr.describe() << " ✓\n";
    std::cout << "  Add describe: " << add.describe() << " ✓\n";

    // Test visitor
    std::string visitor_result = pushInt.visit([](const auto& i) -> std::string {
        using T = std::decay_t<decltype(i)>;
        if constexpr (std::is_same_v<T, Instruction::PushInt>) {
            return "PushInt: " + std::to_string(i.value);
        }
        return "Other";
    });
    assert(visitor_result == "PushInt: 42");
    std::cout << "  Visitor result: " << visitor_result << " ✓\n";

    // Test more instructions
    Instruction loadVar(Instruction::LoadVar{"x"});
    Instruction jump(Instruction::Jump{10});
    Instruction matchInt(Instruction::MatchInt{42});

    std::cout << "  LoadVar describe: " << loadVar.describe() << " ✓\n";
    std::cout << "  Jump describe: " << jump.describe() << " ✓\n";
    std::cout << "  MatchInt describe: " << matchInt.describe() << " ✓\n";

    // Test Bytecode
    Bytecode bytecode;
    bytecode.instructions.push_back(pushInt);
    bytecode.instructions.push_back(add);
    bytecode.instructions.push_back(Instruction(Instruction::Print{}));

    std::cout << "  Bytecode size: " << bytecode.span().size() << " ✓\n";
    for (const auto& instr : bytecode.span()) {
        std::cout << "    - " << instr.describe() << "\n";
    }

    std::cout << "Instruction tests passed! ✅\n\n";
}

int main()
{
    std::cout << "=== New Variant Wrapper Classes Test ===\n\n";

    try {
        testRuntimeValue();
        testInstruction();
        std::cout << "🎉 All tests passed successfully!\n";
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "❌ Test failed with exception: " << e.what() << "\n";
        return 1;
    }
}
