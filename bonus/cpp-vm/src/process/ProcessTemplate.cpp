/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** ProcessTemplate Implementation
*/

#include "process/ProcessTemplate.hpp"

namespace rat
{
    ProcessTemplate::ProcessTemplate(std::string name, std::vector<std::string> params, std::vector<Instruction> body)
        : _name(std::move(name))
        , _params(std::move(params))
        , _body(std::move(body))
    {}

    const std::string& ProcessTemplate::name(void) const noexcept {
        return _name;
    }

    const std::vector<std::string>& ProcessTemplate::params(void) const noexcept {
        return _params;
    }

    const std::vector<Instruction>& ProcessTemplate::body(void) const noexcept {
        return _body;
    }

    size_t ProcessTemplate::paramCount(void) const noexcept {
        return _params.size();
    }

} 
