/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** ProcessTemplate - Immutable value object for process templates
*/

#pragma once

#include <string>
#include <vector>
#include "vm/Instruction.hpp"

namespace rat
{
    /**
     * @class ProcessTemplate
     * @brief Immutable template for creating process instances
     *
     * Value object that encapsulates the blueprint for spawning processes.
     * Follows value semantics for better encapsulation and thread safety.
     *
     * @author Robin Toillon
     */
    class ProcessTemplate {
    public:
        /**
         * @brief Construct a process template
         * @param name Template name (unique identifier)
         * @param params Parameter names that processes will accept
         * @param body Instruction sequence to execute
         */
        ProcessTemplate(std::string name, std::vector<std::string> params, std::vector<Instruction> body);

        // Disable copy for immutability, allow move for storage
        ProcessTemplate(const ProcessTemplate&) = delete;
        ProcessTemplate& operator=(const ProcessTemplate&) = delete;
        ProcessTemplate(ProcessTemplate&&) = default;
        ProcessTemplate& operator=(ProcessTemplate&&) = delete;

        /**
         * @brief Get template name
         * @return Template name
         */
        [[nodiscard]] const std::string& name(void) const noexcept;

        /**
         * @brief Get parameter names
         * @return Parameter names vector
         */
        [[nodiscard]] const std::vector<std::string>& params(void) const noexcept;

        /**
         * @brief Get instruction body
         * @return Instruction sequence
         */
        [[nodiscard]] const std::vector<Instruction>& body(void) const noexcept;

        /**
         * @brief Get parameter count
         * @return Number of parameters
         */
        [[nodiscard]] size_t paramCount(void) const noexcept;

    private:
        std::string _name;
        std::vector<std::string> _params;
        std::vector<Instruction> _body;
    };

} 
