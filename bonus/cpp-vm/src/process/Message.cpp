/*
** EPITECH PROJECT, 2025
** GLaDOS Ratatouille Virtual Machine
** File description:
** Message Implementation
*/

#include "process/Message.hpp"

namespace rat
{
    Message::Message(ProcessId senderPid, RuntimeValue content)
        : _senderPid(senderPid), _content(std::move(content))
    {
    }

    ProcessId Message::sender(void) const noexcept
    {
        return this->_senderPid;
    }

    const RuntimeValue &Message::content(void) const noexcept
    {
        return this->_content;
    }

    RuntimeValue Message::takeContent(void)
    {
        return std::move(this->_content);
    }

}
