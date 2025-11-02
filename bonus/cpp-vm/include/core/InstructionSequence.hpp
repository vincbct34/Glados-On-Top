/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** InstructionSequence - Type alias for bytecode sequences
*/

#pragma once

#include <vector>

namespace rat
{
    struct Instruction;
}

namespace rat::core
{
    using InstructionSequence = std::vector<Instruction>;
} // namespace rat::core
