/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Main Entry Point
*/

#include "vm/VirtualMachine.hpp"

#include <filesystem>
#include <format>
#include <iostream>
#include <string_view>

int main(int argc, char **argv)
{
    if (argc != 2)
        return 84;

    std::string_view arg = argv[1];

    if (arg == "--help" || arg == "-h") {
        std::cerr << std::format(
            "USAGE: {} <file.rtbc>\n"
            "DESCRIPTION:\n"
            "\tfile.rtbc\tfile containing the bytecode to be executed by the GLaDOS VM.\n",
            argv[0]
        );
        return 0;
    }

    if (!std::filesystem::exists(arg)) {
        std::cerr << "Error: File does not exist.\n";
        return 84;
    }

    std::filesystem::path filePath(arg);

    if (filePath.extension() != ".rtbc") {
        std::cerr << "Error: Invalid file extension. Expected .rtbc\n";
        return 84;
    }

    rat::VirtualMachine virtualMachine;
    {
        auto loadingResult = virtualMachine.loadProgram(filePath);

        if (!loadingResult.has_value()) {
            std::cerr << "Loading Error: "
                      << loadingResult.error().describe() << std::endl;
            return 84;
        }
    }
    {
        auto runtimeResult = virtualMachine.run();

        if (!runtimeResult.has_value()) {
            std::cerr << "Runtime Error: "
                      << runtimeResult.error().describe() << std::endl;
            return 84;
        }
    }
    return 0;
}
