/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Performance Benchmarks for VM Instructions
*/

#include <benchmark/benchmark.h>
#include "../src/vm/VirtualMachine.hpp"
#include "../src/core/RuntimeValue.hpp"
#include "../src/core/InstructionStack.hpp"

using namespace rat;

// ========== Stack Operation Benchmarks ==========

static void BM_PushInt(benchmark::State& state) {
    InstructionStack stack;
    
    for (auto _ : state) {
        auto result = stack.push(RuntimeValue(static_cast<int64_t>(42)));
        benchmark::DoNotOptimize(result);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_PushInt);

static void BM_PopInt(benchmark::State& state) {
    InstructionStack stack;
    // Pre-fill stack
    for (int i = 0; i < 1000; ++i) {
        stack.push(RuntimeValue(static_cast<int64_t>(i)));
    }
    
    for (auto _ : state) {
        auto result = stack.pop();
        benchmark::DoNotOptimize(result);
        // Refill to avoid underflow
        if (stack.size() == 0) {
            for (int i = 0; i < 1000; ++i) {
                stack.push(RuntimeValue(static_cast<int64_t>(i)));
            }
        }
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_PopInt);

// ========== Arithmetic Operation Benchmarks ==========

static void BM_AddIntegers(benchmark::State& state) {
    InstructionStack stack;
    
    for (auto _ : state) {
        stack.push(RuntimeValue(static_cast<int64_t>(10)));
        stack.push(RuntimeValue(static_cast<int64_t>(20)));
        
        auto b = stack.pop();
        auto a = stack.pop();
        if (a && b) {
            auto aval = a->getIf<std::int64_t>().value();
            auto bval = b->getIf<std::int64_t>().value();
            stack.push(RuntimeValue(aval + bval));
        }
        
        benchmark::DoNotOptimize(stack);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_AddIntegers);

static void BM_AddFloats(benchmark::State& state) {
    InstructionStack stack;
    
    for (auto _ : state) {
        stack.push(RuntimeValue(3.14));
        stack.push(RuntimeValue(2.86));
        
        auto b = stack.pop();
        auto a = stack.pop();
        if (a && b) {
            auto aval = a->getIf<double>().value();
            auto bval = b->getIf<double>().value();
            stack.push(RuntimeValue(aval + bval));
        }
        
        benchmark::DoNotOptimize(stack);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_AddFloats);

static void BM_AddMixed(benchmark::State& state) {
    InstructionStack stack;
    
    for (auto _ : state) {
        stack.push(RuntimeValue(static_cast<int64_t>(10)));
        stack.push(RuntimeValue(2.5));
        
        auto b = stack.pop();
        auto a = stack.pop();
        if (a && b) {
            double aval = static_cast<double>(a->getIf<std::int64_t>().value());
            double bval = b->getIf<double>().value();
            stack.push(RuntimeValue(aval + bval));
        }
        
        benchmark::DoNotOptimize(stack);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_AddMixed);

// ========== Comparison Operation Benchmarks ==========

static void BM_CompareIntegers(benchmark::State& state) {
    InstructionStack stack;
    
    for (auto _ : state) {
        stack.push(RuntimeValue(static_cast<int64_t>(42)));
        stack.push(RuntimeValue(static_cast<int64_t>(42)));
        
        auto b = stack.pop();
        auto a = stack.pop();
        if (a && b) {
            auto aval = a->getIf<std::int64_t>().value();
            auto bval = b->getIf<std::int64_t>().value();
            stack.push(RuntimeValue(aval == bval));
        }
        
        benchmark::DoNotOptimize(stack);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_CompareIntegers);

// ========== RuntimeValue Benchmarks ==========

static void BM_RuntimeValueCopy(benchmark::State& state) {
    RuntimeValue original(static_cast<int64_t>(42));
    
    for (auto _ : state) {
        RuntimeValue copy = original;
        benchmark::DoNotOptimize(copy);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_RuntimeValueCopy);

static void BM_RuntimeValueMove(benchmark::State& state) {
    for (auto _ : state) {
        RuntimeValue original(static_cast<int64_t>(42));
        RuntimeValue moved = std::move(original);
        benchmark::DoNotOptimize(moved);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_RuntimeValueMove);

// ========== Array Operation Benchmarks ==========

static void BM_CreateArray(benchmark::State& state) {
    const int64_t arraySize = state.range(0);
    
    for (auto _ : state) {
        std::vector<RuntimeValue> elements;
        elements.reserve(arraySize);
        
        for (int64_t i = 0; i < arraySize; ++i) {
            elements.push_back(RuntimeValue(i));
        }
        
        RuntimeValue array(elements, RuntimeValue::Type::Array);
        benchmark::DoNotOptimize(array);
    }
    
    state.SetItemsProcessed(state.iterations() * arraySize);
}
BENCHMARK(BM_CreateArray)->Range(8, 8<<10);

// ========== Pattern Matching Benchmarks ==========

static void BM_PatternMatchInt(benchmark::State& state) {
    PatternMatcher matcher;
    RuntimeValue value(static_cast<int64_t>(42));
    
    for (auto _ : state) {
        bool matches = matcher.matchInt(value, 42);
        benchmark::DoNotOptimize(matches);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_PatternMatchInt);

static void BM_PatternMatchAtom(benchmark::State& state) {
    PatternMatcher matcher;
    RuntimeValue value(std::string("success"), RuntimeValue::Type::Atom);
    
    for (auto _ : state) {
        bool matches = matcher.matchAtom(value, "success");
        benchmark::DoNotOptimize(matches);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_PatternMatchAtom);

// ========== Variable Store Benchmarks ==========

static void BM_VariableSet(benchmark::State& state) {
    VariableStore store;
    RuntimeValue value(static_cast<int64_t>(42));
    
    for (auto _ : state) {
        auto result = store.set("x", value);
        benchmark::DoNotOptimize(result);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_VariableSet);

static void BM_VariableGet(benchmark::State& state) {
    VariableStore store;
    RuntimeValue value(static_cast<int64_t>(42));
    store.set("x", value);
    
    for (auto _ : state) {
        auto result = store.get("x");
        benchmark::DoNotOptimize(result);
    }
    
    state.SetItemsProcessed(state.iterations());
}
BENCHMARK(BM_VariableGet);

// Main function
BENCHMARK_MAIN();
